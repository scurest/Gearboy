#ifndef OPCODE_DEPS_H
#define OPCODE_DEPS_H

#include "definitions.h"

// Instruction dependencies
enum {
    // 0 reserved for no dependency
    // 8-bit registers
    dA = 1,
    dB,
    dC,
    dD,
    dE,
    dH,
    dL,
    // 16-bit registers
    dAF,
    dBC,
    dDE,
    dHL,
    dSP,
    // Addresses
    dAddrBC,
    dAddrDE,
    dAddrHL,
    dAddrSP,
    dAddrImm16,        // 16-bit immediate
    // Memory loads
    dMemBC,
    dMemDE,
    dMemHL,
    dMemSP,
    dMemImm16,         // (16-bit immediate)
    dMemFF00PlusImm8,  // ($FF00 + 8-bit immediate)
    dMemFF00PlusC,     // ($FF00 + C)
    // Flags
    dFlagZ,
    dFlagC,
    dFlagSHC,          // for DAA
};

static const u8 kOPCodeDeps[256][2] = {
    { 0 },                    // NOP
    { 0 },                    // LD BC,$%04X
    { dAddrBC, dA },          // LD (BC),A
    { dBC },                  // INC BC
    { dB },                   // INC B
    { dB },                   // DEC B
    { 0 },                    // LD B,$%02X
    { dA },                   // RLCA
    { dAddrImm16, dSP },      // LD ($%04X),SP
    { dHL, dBC },             // ADD HL,BC
    { dMemBC },               // LD A,(BC)
    { dBC },                  // DEC BC
    { dC },                   // INC C
    { dC },                   // DEC C
    { 0 },                    // LD C,$%02X
    { dA },                   // RRCA
    { 0 },                    // STOP

    { 0 },                    // LD DE,$%04X
    { dAddrDE, dA },          // LD (DE),A
    { dDE },                  // INC DE
    { dD },                   // INC D
    { dD },                   // DEC D
    { 0 },                    // LD D,$%02X
    { dA },                   // RLA
    { 0 },                    // JR $%04X  [%+d]
    { dHL, dDE },             // ADD HL,DE
    { dMemDE },               // LD A,(DE)
    { dDE },                  // DEC DE
    { dE },                   // INC E
    { dE },                   // DEC E
    { 0 },                    // LD E,$%02X
    { dA },                   // RRA
    { dFlagZ },               // JR NZ,$%04X  [%+d]

    { 0 },                    // LD HL,$%04X
    { dAddrHL, dA },          // LD (HL+),A
    { dHL },                  // INC HL
    { dH },                   // INC H
    { dH },                   // DEC H
    { 0 },                    // LD H,$%02X
    { dA, dFlagSHC },         // DAA
    { dFlagZ },               // JR Z,$%04X  [%+d]
    { dHL },                  // ADD HL,HL
    { dMemHL },               // LD A,(HL+)
    { dHL },                  // DEC HL
    { dL },                   // INC L
    { dL },                   // DEC L
    { 0 },                    // LD L,$%02X
    { dA },                   // CPL
    { dFlagC },               // JR NC,$%04X  [%+d]

    { 0 },                    // LD SP,$%04X
    { dAddrHL, dA },          // LD (HL-),A
    { dSP },                  // INC SP
    { dMemHL },               // INC (HL)
    { dMemHL },               // DEC (HL)
    { dAddrHL },              // LD (HL),$%02X
    { 0 },                    // SCF
    { dFlagC },               // JR C,$%04X  [%+d]
    { dHL, dSP },             // ADD HL,SP
    { dMemHL },               // LD A,(HL-)
    { dSP },                  // DEC SP
    { dA },                   // INC A
    { dA },                   // DEC A
    { 0 },                    // LD A,$%02X
    { dFlagC },               // CCF
    { dB },                   // LD B,B

    { dC },                   // LD B,C
    { dD },                   // LD B,D
    { dE },                   // LD B,E
    { dH },                   // LD B,H
    { dL },                   // LD B,L
    { dMemHL },               // LD B,(HL)
    { dA },                   // LD B,A
    { dB },                   // LD C,B
    { dC },                   // LD C,C
    { dD },                   // LD C,D
    { dE },                   // LD C,E
    { dH },                   // LD C,H
    { dL },                   // LD C,L
    { dMemHL },               // LD C,(HL)
    { dA },                   // LD C,A
    { dB },                   // LD D,B

    { dC },                   // LD D,C
    { dD },                   // LD D,D
    { dE },                   // LD D,E
    { dH },                   // LD D,H
    { dL },                   // LD D,L
    { dMemHL },               // LD D,(HL)
    { dA },                   // LD D,A
    { dB },                   // LD E,B
    { dC },                   // LD E,C
    { dD },                   // LD E,D
    { dE },                   // LD E,E
    { dH },                   // LD E,H
    { dL },                   // LD E,L
    { dMemHL },               // LD E,(HL)
    { dA },                   // LD E,A
    { dB },                   // LD H,B

    { dC },                   // LD H,C
    { dD },                   // LD H,D
    { dE },                   // LD H,E
    { dH },                   // LD H,H
    { dL },                   // LD H,L
    { dMemHL },               // LD H,(HL)
    { dA },                   // LD H,A
    { dB },                   // LD L,B
    { dC },                   // LD L,C
    { dD },                   // LD L,D
    { dE },                   // LD L,E
    { dH },                   // LD L,H
    { dL },                   // LD L,L
    { dMemHL },               // LD L,(HL)
    { dA },                   // LD L,A
    { dAddrHL, dB },          // LD (HL),B

    { dAddrHL, dC },          // LD (HL),C
    { dAddrHL, dD },          // LD (HL),D
    { dAddrHL, dE },          // LD (HL),E
    { dAddrHL },              // LD (HL),H
    { dAddrHL },              // LD (HL),L
    { 0 },                    // HALT
    { dAddrHL, dA },          // LD (HL),A
    { dB },                   // LD A,B
    { dC },                   // LD A,C
    { dD },                   // LD A,D
    { dE },                   // LD A,E
    { dH },                   // LD A,H
    { dL },                   // LD A,L
    { dMemHL },               // LD A,(HL)
    { dA },                   // LD A,A
    { dA, dB },               // ADD A,B

    { dA, dC },               // ADD A,C
    { dA, dD },               // ADD A,D
    { dA, dE },               // ADD A,E
    { dA, dH },               // ADD A,H
    { dA, dL },               // ADD A,L
    { dA, dMemHL },           // ADD A,(HL)
    { dA },                   // ADD A,A
    { dA, dB },               // ADC A,B
    { dA, dC },               // ADC A,C
    { dA, dD },               // ADC A,D
    { dA, dE },               // ADC A,E
    { dA, dH },               // ADC A,H
    { dA, dL },               // ADC A,L
    { dA, dMemHL },           // ADC A,(HL)
    { dA },                   // ADC A,A
    { dA, dB },               // SUB B

    { dA, dC },               // SUB C
    { dA, dD },               // SUB D
    { dA, dE },               // SUB E
    { dA, dH },               // SUB H
    { dA, dL },               // SUB L
    { dA, dMemHL },           // SUB (HL)
    { dA },                   // SUB A
    { dA, dB },               // SBC A,B
    { dA, dC },               // SBC A,C
    { dA, dD },               // SBC A,D
    { dA, dE },               // SBC A,E
    { dA, dH },               // SBC A,H
    { dA, dL },               // SBC A,L
    { dA, dMemHL },           // SBC A,(HL)
    { dA },                   // SBC A,A
    { dA, dB },               // AND B

    { dA, dC },               // AND C
    { dA, dD },               // AND D
    { dA, dE },               // AND E
    { dA, dH },               // AND H
    { dA, dL },               // AND L
    { dA, dMemHL },           // AND (HL)
    { dA },                   // AND A
    { dA, dB },               // XOR B
    { dA, dC },               // XOR C
    { dA, dD },               // XOR D
    { dA, dE },               // XOR E
    { dA, dH },               // XOR H
    { dA, dL },               // XOR L
    { dA, dMemHL },           // XOR (HL)
    { dA },                   // XOR A
    { dA, dB },               // OR B

    { dA, dC },               // OR C
    { dA, dD },               // OR D
    { dA, dE },               // OR E
    { dA, dH },               // OR H
    { dA, dL },               // OR L
    { dA, dMemHL },           // OR (HL)
    { dA },                   // OR A
    { dA, dB },               // CP B
    { dA, dC },               // CP C
    { dA, dD },               // CP D
    { dA, dE },               // CP E
    { dA, dH },               // CP H
    { dA, dL },               // CP L
    { dA, dMemHL },           // CP (HL)
    { dA },                   // CP A
    { dMemSP, dFlagZ },       // RET NZ

    { dMemSP },               // POP BC
    { dFlagZ },               // JP NZ,$%04X
    { 0 },                    // JP $%04X
    { dAddrSP, dFlagZ },      // CALL NZ,$%04X
    { dAddrSP, dBC },         // PUSH BC
    { dA },                   // ADD A,$%02X
    { dAddrSP },              // RST
    { dMemSP, dFlagZ },       // RET Z
    { dMemSP },               // RET
    { dFlagZ },               // JP Z,$%04X
    { 0 },                    // cb opcode
    { dAddrSP, dFlagZ },      // CALL Z,$%04X
    { dAddrSP },              // CALL $%04X
    { dA },                   // ADC A,$%02X
    { dAddrSP },              // RST $08
    { dMemSP, dFlagC },       // RET NC

    { dMemSP },               // POP DE
    { dFlagC },               // JP NC,$%04X
    { 0 },                    // [UNUSED]
    { dAddrSP, dFlagC },      // CALL NC,$%04X
    { dAddrSP, dDE },         // PUSH DE
    { dA },                   // SUB $%02X
    { dAddrSP },              // RST $10
    { dMemSP, dFlagC },       // RET C
    { 0 },                    // RETI
    { dFlagC },               // JP C,$%04X
    { 0 },                    // [UNUSED]
    { dAddrSP, dFlagC },      // CALL C,$%04X
    { 0 },                    // [UNUSED]
    { dA },                   // SBC A,$%02X
    { dAddrSP },              // RST $18
    { dA },                   // LD ($FF00+$%02X),A  [%s]

    { dMemSP },               // POP HL
    { dC, dA },               // LD ($FF00+C),A
    { 0 },                    // [UNUSED]
    { 0 },                    // [UNUSED]
    { dAddrSP, dHL },         // PUSH HL
    { dA },                   // AND $%02X
    { dAddrSP },              // RST $20
    { dSP },                  // ADD SP,%+d
    { dAddrHL },              // JP (HL)
    { dAddrImm16, dA },       // LD ($%04X),A
    { 0 },                    // [UNUSED]
    { 0 },                    // [UNUSED]
    { 0 },                    // [UNUSED]
    { dA },                   // XOR $%02X
    { dAddrSP },              // RST $28
    { dMemFF00PlusImm8 },     // LD A,($FF00+$%02X)  [%s]

    { dMemSP },               // POP AF
    { dMemFF00PlusC },        // LD A,($FF00+C)
    { 0 },                    // DI
    { 0 },                    // [UNUSED]
    { dAddrSP, dAF },         // PUSH AF
    { dA },                   // OR $%02X
    { dAddrSP },              // RST $30
    { 0 },                    // LD HL,(SP%+d)
    { dHL },                  // LD SP,HL
    { dMemImm16 },            // LD A,($%04X)
    { 0 },                    // EI
    { 0 },                    // [UNUSED]
    { 0 },                    // [UNUSED]
    { dA },                   // CP $%02X
    { dAddrSP }               // RST $38
};

static const u8 kOPCodeCBDeps[256][2] = {
    { dFlagC, dB },           // RLC B
    { dFlagC, dC },           // RLC C
    { dFlagC, dD },           // RLC D
    { dFlagC, dE },           // RLC E
    { dFlagC, dH },           // RLC H
    { dFlagC, dL },           // RLC L
    { dFlagC, dMemHL },       // RLC (HL)
    { dFlagC, dA },           // RLC A
    { dFlagC, dB },           // RRC B
    { dFlagC, dC },           // RRC C
    { dFlagC, dD },           // RRC D
    { dFlagC, dE },           // RRC E
    { dFlagC, dH },           // RRC H
    { dFlagC, dL },           // RRC L
    { dFlagC, dMemHL },       // RRC (HL)
    { dFlagC, dA },           // RRC A
    { dB },                   // RL B

    { dC },                   // RL C
    { dD },                   // RL D
    { dE },                   // RL E
    { dH },                   // RL H
    { dL },                   // RL L
    { dMemHL },               // RL (HL)
    { dA },                   // RL A
    { dB },                   // RR B
    { dC },                   // RR C
    { dD },                   // RR D
    { dE },                   // RR E
    { dH },                   // RR H
    { dL },                   // RR L
    { dMemHL },               // RR (HL)
    { dA },                   // RR A
    { dB },                   // SLA B

    { dC },                   // SLA C
    { dD },                   // SLA D
    { dE },                   // SLA E
    { dH },                   // SLA H
    { dL },                   // SLA L
    { dMemHL },               // SLA (HL)
    { dA },                   // SLA A
    { dB },                   // SRA B
    { dC },                   // SRA C
    { dD },                   // SRA D
    { dE },                   // SRA E
    { dH },                   // SRA H
    { dL },                   // SRA L
    { dMemHL },               // SRA (HL)
    { dA },                   // SRA A
    { dB },                   // SWAP B

    { dC },                   // SWAP C
    { dD },                   // SWAP D
    { dE },                   // SWAP E
    { dH },                   // SWAP H
    { dL },                   // SWAP L
    { dMemHL },               // SWAP (HL)
    { dA },                   // SWAP A
    { dB },                   // SRL B
    { dC },                   // SRL C
    { dD },                   // SRL D
    { dE },                   // SRL E
    { dH },                   // SRL H
    { dL },                   // SRL L
    { dMemHL },               // SRL (HL)
    { dA },                   // SRL A
    { dB },                   // BIT 0 B

    { dC },                   // BIT 0 C
    { dD },                   // BIT 0 D
    { dE },                   // BIT 0 E
    { dH },                   // BIT 0 H
    { dL },                   // BIT 0 L
    { dMemHL },               // BIT 0 (HL)
    { dA },                   // BIT 0 A
    { dB },                   // BIT 1 B
    { dC },                   // BIT 1 C
    { dD },                   // BIT 1 D
    { dE },                   // BIT 1 E
    { dH },                   // BIT 1 H
    { dL },                   // BIT 1 L
    { dMemHL },               // BIT 1 (HL)
    { dA },                   // BIT 1 A
    { dB },                   // BIT 2 B

    { dC },                   // BIT 2 C
    { dD },                   // BIT 2 D
    { dE },                   // BIT 2 E
    { dH },                   // BIT 2 H
    { dL },                   // BIT 2 L
    { dMemHL },               // BIT 2 (HL)
    { dA },                   // BIT 2 A
    { dB },                   // BIT 3 B
    { dC },                   // BIT 3 C
    { dD },                   // BIT 3 D
    { dE },                   // BIT 3 E
    { dH },                   // BIT 3 H
    { dL },                   // BIT 3 L
    { dMemHL },               // BIT 3 (HL)
    { dA },                   // BIT 3 A
    { dB },                   // BIT 4 B

    { dC },                   // BIT 4 C
    { dD },                   // BIT 4 D
    { dE },                   // BIT 4 E
    { dH },                   // BIT 4 H
    { dL },                   // BIT 4 L
    { dMemHL },               // BIT 4 (HL)
    { dA },                   // BIT 4 A
    { dB },                   // BIT 5 B
    { dC },                   // BIT 5 C
    { dD },                   // BIT 5 D
    { dE },                   // BIT 5 E
    { dH },                   // BIT 5 H
    { dL },                   // BIT 5 L
    { dMemHL },               // BIT 5 (HL)
    { dA },                   // BIT 5 A
    { dB },                   // BIT 6 B

    { dC },                   // BIT 6 C
    { dD },                   // BIT 6 D
    { dE },                   // BIT 6 E
    { dH },                   // BIT 6 H
    { dL },                   // BIT 6 L
    { dMemHL },               // BIT 6 (HL)
    { dA },                   // BIT 6 A
    { dB },                   // BIT 7 B
    { dC },                   // BIT 7 C
    { dD },                   // BIT 7 D
    { dE },                   // BIT 7 E
    { dH },                   // BIT 7 H
    { dL },                   // BIT 7 L
    { dMemHL },               // BIT 7 (HL)
    { dA },                   // BIT 7 A
    { dB },                   // RES 0 B

    { dC },                   // RES 0 C
    { dD },                   // RES 0 D
    { dE },                   // RES 0 E
    { dH },                   // RES 0 H
    { dL },                   // RES 0 L
    { dMemHL },               // RES 0 (HL)
    { dA },                   // RES 0 A
    { dB },                   // RES 1 B
    { dC },                   // RES 1 C
    { dD },                   // RES 1 D
    { dE },                   // RES 1 E
    { dH },                   // RES 1 H
    { dL },                   // RES 1 L
    { dMemHL },               // RES 1 (HL)
    { dA },                   // RES 1 A
    { dB },                   // RES 2 B

    { dC },                   // RES 2 C
    { dD },                   // RES 2 D
    { dE },                   // RES 2 E
    { dH },                   // RES 2 H
    { dL },                   // RES 2 L
    { dMemHL },               // RES 2 (HL)
    { dA },                   // RES 2 A
    { dB },                   // RES 3 B
    { dC },                   // RES 3 C
    { dD },                   // RES 3 D
    { dE },                   // RES 3 E
    { dH },                   // RES 3 H
    { dL },                   // RES 3 L
    { dMemHL },               // RES 3 (HL)
    { dA },                   // RES 3 A
    { dB },                   // RES 4 B

    { dC },                   // RES 4 C
    { dD },                   // RES 4 D
    { dE },                   // RES 4 E
    { dH },                   // RES 4 H
    { dL },                   // RES 4 L
    { dMemHL },               // RES 4 (HL)
    { dA },                   // RES 4 A
    { dB },                   // RES 5 B
    { dC },                   // RES 5 C
    { dD },                   // RES 5 D
    { dE },                   // RES 5 E
    { dH },                   // RES 5 H
    { dL },                   // RES 5 L
    { dMemHL },               // RES 5 (HL)
    { dA },                   // RES 5 A
    { dB },                   // RES 6 B

    { dC },                   // RES 6 C
    { dD },                   // RES 6 D
    { dE },                   // RES 6 E
    { dH },                   // RES 6 H
    { dL },                   // RES 6 L
    { dMemHL },               // RES 6 (HL)
    { dA },                   // RES 6 A
    { dB },                   // RES 7 B
    { dC },                   // RES 7 C
    { dD },                   // RES 7 D
    { dE },                   // RES 7 E
    { dH },                   // RES 7 H
    { dL },                   // RES 7 L
    { dMemHL },               // RES 7 (HL)
    { dA },                   // RES 7 A
    { dB },                   // SET 0 B

    { dC },                   // SET 0 C
    { dD },                   // SET 0 D
    { dE },                   // SET 0 E
    { dH },                   // SET 0 H
    { dL },                   // SET 0 L
    { dMemHL },               // SET 0 (HL)
    { dA },                   // SET 0 A
    { dB },                   // SET 1 B
    { dC },                   // SET 1 C
    { dD },                   // SET 1 D
    { dE },                   // SET 1 E
    { dH },                   // SET 1 H
    { dL },                   // SET 1 L
    { dMemHL },               // SET 1 (HL)
    { dA },                   // SET 1 A
    { dB },                   // SET 2 B

    { dC },                   // SET 2 C
    { dD },                   // SET 2 D
    { dE },                   // SET 2 E
    { dH },                   // SET 2 H
    { dL },                   // SET 2 L
    { dMemHL },               // SET 2 (HL)
    { dA },                   // SET 2 A
    { dB },                   // SET 3 B
    { dC },                   // SET 3 C
    { dD },                   // SET 3 D
    { dE },                   // SET 3 E
    { dH },                   // SET 3 H
    { dL },                   // SET 3 L
    { dMemHL },               // SET 3 (HL)
    { dA },                   // SET 3 A
    { dB },                   // SET 4 B

    { dC },                   // SET 4 C
    { dD },                   // SET 4 D
    { dE },                   // SET 4 E
    { dH },                   // SET 4 H
    { dL },                   // SET 4 L
    { dMemHL },               // SET 4 (HL)
    { dA },                   // SET 4 A
    { dB },                   // SET 5 B
    { dC },                   // SET 5 C
    { dD },                   // SET 5 D
    { dE },                   // SET 5 E
    { dH },                   // SET 5 H
    { dL },                   // SET 5 L
    { dMemHL },               // SET 5 (HL)
    { dA },                   // SET 5 A
    { dB },                   // SET 6 B

    { dC },                   // SET 6 C
    { dD },                   // SET 6 D
    { dE },                   // SET 6 E
    { dH },                   // SET 6 H
    { dL },                   // SET 6 L
    { dMemHL },               // SET 6 (HL)
    { dA },                   // SET 6 A
    { dB },                   // SET 7 B
    { dC },                   // SET 7 C
    { dD },                   // SET 7 D
    { dE },                   // SET 7 E
    { dH },                   // SET 7 H
    { dL },                   // SET 7 L
    { dMemHL },               // SET 7 (HL)
    { dA }                    // SET 7 A
};

#endif  /* OPCODE_DEPS_H */
