#include <stdio.h>
#include "Processor.h"
#include "Memory.h"
#include "opcode_names.h"
#include "opcode_deps.h"

void Processor::StartTrace()
{
    if (m_pTraceFile)
        return;

    m_pTraceFile = fopen("Gearboy_log.txt", "wb");
    if (!m_pTraceFile)
    {
        Log("CPU Trace: Error opening Gearboy_log.txt");
    }

    m_iTraceSize = 0;
}

void Processor::StopTrace()
{
    if (m_pTraceFile)
    {
        fclose(m_pTraceFile);
        m_pTraceFile = nullptr;
        m_iTraceSize = 0;
    }
}

bool Processor::IsTracing() const
{
    return m_pTraceFile != nullptr;
}

unsigned Processor::GetTraceSizeInMB() const
{
    return m_iTraceSize / ((u64)1024 * (u64)1024);
}

void Processor::Trace_Log(const char* message)
{
    if (!IsTracing())
        return;

    int result = fprintf(m_pTraceFile, "%s\n", message);
    if (result < 0)
    {
        Log("CPU Trace: Error writing to trace file. Stopping trace");
        StopTrace();
        return;
    }

    m_iTraceSize += result;
}

void Processor::Trace_Instruction(u16 address)
{
    char buf[96];
    int len = 0;

    // Print PC
    len += Trace_PrintAddress(buf + len, address);
    len += sprintf(buf + len, "\t");

    u8 opcodes[4];
    for (int i = 0; i < 4; i++)
        opcodes[i] = m_pMemory->Read(address + i);

    u8 opcode = opcodes[0];
    bool cb = opcode == 0xCB;
    if (cb)
        opcode = opcodes[1];

    const stOPCodeInfo& info = cb ? kOPCodeCBNames[opcode] : kOPCodeNames[opcode];

    // Print disassembly
    u16 imm16;
    switch (info.type)
    {
        case 0:
            len += sprintf(buf + len, "%s", info.name);
            break;
        case 1:
            len += sprintf(buf + len, info.name, opcodes[1]);
            break;
        case 2:
            imm16 = (opcodes[2] << 8) | opcodes[1];
            len += sprintf(buf + len, info.name, imm16);
            break;
        case 3:
            len += sprintf(buf + len, info.name, (s8)opcodes[1]);
            break;
        case 4:
            imm16 = address + info.size + (s8)opcodes[1];
            len += sprintf(buf + len, info.name, imm16, (s8)opcodes[1]);
            break;
        case 5:
            len += sprintf(buf + len, info.name, opcodes[1], kRegisterNames[opcodes[1]]);
            break;
        default:
            len += sprintf(buf + len, "PARSE ERROR");
    }

    const u8* deps = cb ? kOPCodeCBDeps[opcode] : kOPCodeDeps[opcode];

    // Print dependencies
    for (int i = 0; i < 2; i++)
    {
        if (deps[i] == 0)
            break;

        len += sprintf(buf + len, "  ");
        if (i == 0)
        {
            // Align to column 40
            while (len < 40)
                buf[len++] = ' ';
            buf[len] = 0;
        }

        len += Trace_PrintDependency(buf + len, opcodes, deps[i]);
    }

    Trace_Log(buf);
}

// Prints an address like "VRA1:8B49". 8B49 is the address, "VRA" is
// the name of the memory region (VRAM), and "1" is the current bank
// index for regions with switchable banks.
//
// This is the same format as bgb.
int Processor::Trace_PrintAddress(char str[10], u16 addr)
{
    int bank = -1;
    const char* name;

    if (addr < 0x4000)
    {
        name = "ROM ";
        bank = m_pMemory->GetCurrentRule()->GetCurrentRomBank0Index();
    }
    else if (addr < 0x8000)
    {
        name = "ROM ";
        bank = m_pMemory->GetCurrentRule()->GetCurrentRomBank1Index();
    }
    else if (addr < 0xA000)
    {
        name = "VRAM";
        bank = m_pMemory->GetCurrentLCDRAMBank();
    }
    else if (addr < 0xC000)
    {
        name = "SRAM";
        bank = m_pMemory->GetCurrentRule()->GetCurrentRamBankIndex();
    }
    else if (addr < 0xD000)
        name = "WRA0";
    else if (addr < 0xE000)
    {
        name = "WRAM";
        bank = m_pMemory->GetCurrentCGBRAMBank();
    }
    else if (addr < 0xF000)
        name = "ECH0";
    else if (addr < 0xFE00)
    {
        name = "ECHO";
        bank = m_pMemory->GetCurrentCGBRAMBank();
    }
    else if (addr < 0xFEA0)
        name = "OAM ";
    else if (addr < 0xFF00)
        name = "----";
    else if (addr < 0xFF80)
        name = "I/O ";
    else if (addr < 0xFFFF)
        name = "HRAM";
    else  // addr == 0xFFFF
        name = "I/O ";

    if (bank < 0)
        return sprintf(str, "%s:%04X", name, addr);
    else if (bank < 16)
        return sprintf(str, "%.3s%X:%04X", name, bank, addr);
    else
        return sprintf(str, "%.2s%X:%04X", name, (u8)bank, addr);
}

int Processor::Trace_PrintDependency(char* str, u8 opcodes[4], u8 dep)
{
    u16 tmp;
    char addr[10];

    switch (dep)
    {
        case dA:
            return sprintf(str, "A=$%02X", AF.GetHigh());
        case dB:
            return sprintf(str, "B=$%02X", BC.GetHigh());
        case dC:
            return sprintf(str, "C=$%02X", BC.GetLow());
        case dD:
            return sprintf(str, "D=$%02X", DE.GetHigh());
        case dE:
            return sprintf(str, "E=$%02X", DE.GetLow());
        case dH:
            return sprintf(str, "H=$%02X", HL.GetHigh());
        case dL:
            return sprintf(str, "L=$%02X", HL.GetLow());

        case dAF:
            return sprintf(str, "AF=$%04X", AF.GetValue());
        case dBC:
            return sprintf(str, "BC=$%04X", BC.GetValue());
        case dDE:
            return sprintf(str, "DE=$%04X", DE.GetValue());
        case dHL:
            return sprintf(str, "HL=$%04X", HL.GetValue());
        case dSP:
            return sprintf(str, "SP=$%04X", SP.GetValue());

        case dAddrBC:
            Trace_PrintAddress(addr, BC.GetValue());
            return sprintf(str, "BC=%s", addr);
        case dAddrDE:
            Trace_PrintAddress(addr, DE.GetValue());
            return sprintf(str, "DE=%s", addr);
        case dAddrHL:
            Trace_PrintAddress(addr, HL.GetValue());
            return sprintf(str, "HL=%s", addr);
        case dAddrSP:
            Trace_PrintAddress(addr, SP.GetValue());
            return sprintf(str, "SP=%s", addr);
        case dAddrImm16:
            tmp = (opcodes[2] << 8) | opcodes[1];
            Trace_PrintAddress(addr, tmp);
            return sprintf(str, "addr=%s", addr);

        case dMemBC:
            Trace_PrintAddress(addr, BC.GetValue());
            return sprintf(str, "(%s)=$%02X", addr, m_pMemory->Read(BC.GetValue()));
        case dMemDE:
            Trace_PrintAddress(addr, DE.GetValue());
            return sprintf(str, "(%s)=$%02X", addr, m_pMemory->Read(DE.GetValue()));
        case dMemHL:
            Trace_PrintAddress(addr, HL.GetValue());
            return sprintf(str, "(%s)=$%02X", addr, m_pMemory->Read(HL.GetValue()));
        case dMemSP:
            tmp = m_pMemory->Read(SP.GetValue());
            tmp |= m_pMemory->Read(SP.GetValue() + 1) << 8;
            Trace_PrintAddress(addr, SP.GetValue());
            return sprintf(str, "(%s)=$%04X", addr, tmp);
        case dMemImm16:
            tmp = (opcodes[2] << 8) | opcodes[1];
            Trace_PrintAddress(addr, tmp);
            return sprintf(str, "(%s)=$%02X", addr, m_pMemory->Read(tmp));
        case dMemFF00PlusImm8:
            tmp = 0xFF00 + opcodes[1];
            Trace_PrintAddress(addr, tmp);
            return sprintf(str, "(%s)=$%02X", addr, m_pMemory->Read(tmp));
        case dMemFF00PlusC:
            tmp = 0xFF00 + BC.GetLow();
            Trace_PrintAddress(addr, tmp);
            return sprintf(str, "(%s)=$%02X", addr, m_pMemory->Read(tmp));

        case dFlagC:
            return sprintf(str, "C=%d", (AF.GetLow() & FLAG_CARRY) != 0);
        case dFlagZ:
            return sprintf(str, "Z=%d", (AF.GetLow() & FLAG_ZERO) != 0);
        case dFlagSHC:
            return sprintf(
                str,
                "SHC=%d%d%d",
                (AF.GetLow() & FLAG_SUB) != 0,
                (AF.GetLow() & FLAG_HALF) != 0,
                (AF.GetLow() & FLAG_CARRY) != 0
            );

        default:
            return 0;
    }
}
