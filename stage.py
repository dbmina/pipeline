import sys

from consts import *
from isa import *
from components import *                                                                                                
from program import *
from pipe import *


#--------------------------------------------------------------------------
#   Control signal table
#--------------------------------------------------------------------------

csignals = {
    LW     : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_ADD  , WB_MEM, REN_1, MEN_1, M_XRD, MT_W, ],
    SW     : [ Y, BR_N  , OP1_RS1, OP2_IMS, OEN_1, OEN_1, ALU_ADD  , WB_X  , REN_0, MEN_1, M_XWR, MT_W, ],
    AUIPC  : [ Y, BR_N  , OP1_PC,  OP2_IMU, OEN_0, OEN_0, ALU_ADD  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    LUI    : [ Y, BR_N  , OP1_X,   OP2_IMU, OEN_0, OEN_0, ALU_COPY2, WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    ADDI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_ADD  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],

    SLLI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_SLL  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SLTI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_SLT  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SLTIU  : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_SLTU , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    XORI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_XOR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SRLI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_SRL  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],

    SRAI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_SRA  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    ORI    : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_OR   , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    ANDI   : [ Y, BR_N  , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_AND  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    ADD    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_ADD  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SUB    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SUB  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],

    SLL    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SLL  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SLT    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SLT  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SLTU   : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SLTU , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    XOR    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_XOR  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    SRL    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SRL  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],

    SRA    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_SRA  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    OR     : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_OR   , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    AND    : [ Y, BR_N  , OP1_RS1, OP2_RS2, OEN_1, OEN_1, ALU_AND  , WB_ALU, REN_1, MEN_0, M_X  , MT_X, ],
    JALR   : [ Y, BR_JR , OP1_RS1, OP2_IMI, OEN_1, OEN_0, ALU_ADD  , WB_PC4, REN_1, MEN_0, M_X  , MT_X, ],
    JAL    : [ Y, BR_J  , OP1_RS1, OP2_IMJ, OEN_0, OEN_0, ALU_X    , WB_PC4, REN_1, MEN_0, M_X  , MT_X, ],

    BEQ    : [ Y, BR_EQ , OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SEQ  , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    BNE    : [ Y, BR_NE , OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SEQ  , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    BLT    : [ Y, BR_LT , OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SLT  , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    BGE    : [ Y, BR_GE , OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SLT  , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    BLTU   : [ Y, BR_LTU, OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SLTU , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],

    BGEU   : [ Y, BR_GEU, OP1_RS1, OP2_IMB, OEN_1, OEN_1, ALU_SLTU , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    ECALL  : [ Y, BR_N  , OP1_X  , OP2_X  , OEN_0, OEN_0, ALU_X    , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
    EBREAK : [ Y, BR_N  , OP1_X  , OP2_X  , OEN_0, OEN_0, ALU_X    , WB_X  , REN_0, MEN_0, M_X  , MT_X, ],
}
class IF(Pipe):

    # Pipeline registers ------------------------------

    reg_pc          = WORD(0)       # IF.reg_pc

    #--------------------------------------------------


    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc                 # Pipe.ID.pc
        #   self.inst               # Pipe.ID.inst
        #   self.exception          # Pipe.ID.exception
        #   self.pc_next            # Pipe.ID.pc_next
        #   self.pcplus4            # Pipe.ID.pcplus4
        #
        #----------------------------------------------

    def compute(self):

        # DO NOT TOUCH -----------------------------------------------
        # Read out pipeline register values

        self.pc     = IF.reg_pc
        # Fetch an instruction from instruction memory (imem)

        self.inst, status = Pipe.cpu.imem.access(True, self.pc, 0, M_XRD)

        # Handle exception during imem access
        if not status:
            self.exception = EXC_IMEM_ERROR
            self.inst = BUBBLE
        else:
            self.exception = EXC_NONE

        opcode=RISCV.opcode(self.inst)
        self.pcplus4=Pipe.cpu.adder_pcplus4.op(self.pc, 4)
        self.pc_next=self.pcplus4

        if opcode!=ILLEGAL:
            csa=csignals[opcode]
            self.c_br_type=csa[CS_BR_TYPE]

        if self.c_br_type!=0 and self.c_br_type!=7 and self.c_br_type!=8:

            imm_b=RISCV.imm_b(self.inst)
            self.pc_next=Pipe.cpu.adder_brtarget.op(IF.reg_pc, imm_b)

        elif self.c_br_type==7:
            imm_j=RISCV.imm_j(self.inst)
            self.pc_next=Pipe.cpu.adder_brtarget.op(IF.reg_pc, imm_j)
        elif self.c_br_type==8:
            self.pc_next=self.pcplus4
        elif Pipe.ID.pc_sel==PC_4:
            self.pc_next=self.pcplus4
        else:
            self.pc_next=WORD(0)


    def update(self):



        IF.reg_pc         = self.pc_next
        ID.reg_pcplus4    = self.pcplus4
        ID.reg_pc         = self.pc
        ID.reg_inst       = self.inst
        ID.reg_exception  = self.exception

        # DO NOT TOUCH -----------------------------------------------
        Pipe.log(S_IF, self.pc, self.inst, self.log())
        #-------------------------------------------------------------

    def log(self):
        return("# inst=0x%08x, pc_next=0x%08x" % (self.inst, self.pc_next))
    #-----------------------------------------------------------------


#--------------------------------------------------------------------------
#   ID: Instruction decode stage
#--------------------------------------------------------------------------

class ID(Pipe):


    # Pipeline registers ------------------------------
    reg_pcplus4     = WORD(0)
    reg_pc          = WORD(0)           # ID.reg_pc
    reg_inst        = WORD(BUBBLE)      # ID.reg_inst
    reg_exception   = WORD(EXC_NONE)    # ID.reg_exception

    #--------------------------------------------------


    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc                 # Pipe.ID.pc
        #   self.inst               # Pipe.ID.inst
    
    def compute(self):

        # Readout pipeline register values
        self.pc         = ID.reg_pc
        self.inst       = ID.reg_inst
        self.exception  = ID.reg_exception
        self.pcplus4    = ID.reg_pcplus4
        self.rs1        = RISCV.rs1(self.inst)
        self.rs2        = RISCV.rs2(self.inst)
        self.rd         = RISCV.rd(self.inst)

        imm_i           = RISCV.imm_i(self.inst)
        imm_s           = RISCV.imm_s(self.inst)
        imm_b           = RISCV.imm_b(self.inst)
        imm_u           = RISCV.imm_u(self.inst)
        imm_j           = RISCV.imm_j(self.inst)



        # Generate control signals

        # DO NOT TOUCH------------------------------------------------
        opcode          = RISCV.opcode(self.inst)
        if opcode in [ EBREAK, ECALL ]:
            self.exception |= EXC_EBREAK
        elif opcode == ILLEGAL:
            self.exception |= EXC_ILLEGAL_INST
            self.inst = BUBBLE
            opcode = RISCV.opcode(self.inst)

        cs = csignals[opcode]
        self.c_br_type  = cs[CS_BR_TYPE]
        self.c_op1_sel  = cs[CS_OP1_SEL]
        self.c_op2_sel  = cs[CS_OP2_SEL]
        self.c_alu_fun  = cs[CS_ALU_FUN]
        self.c_wb_sel   = cs[CS_WB_SEL]
        self.c_rf_wen   = cs[CS_RF_WEN]
        self.c_dmem_en  = cs[CS_MEM_EN]
        self.c_dmem_rw  = cs[CS_MEM_FCN]
        self.c_rs1_oen  = cs[CS_RS1_OEN]
        self.c_rs2_oen  = cs[CS_RS2_OEN]

        # Any instruction with an exception becomes BUBBLE as it enters the MM stage. (except EBREAK)
        # All the following instructions after exception become BUBBLEs too.
        self.MM_bubble = (Pipe.EX.exception and (Pipe.EX.exception != EXC_EBREAK)) or (Pipe.MM.exception)
        #-------------------------------------------------------------

        # Prepare immediate values
        self.imm        = imm_i         if self.c_op2_sel == OP2_IMI      else \
                          imm_s         if self.c_op2_sel == OP2_IMS      else \
                          imm_b         if self.c_op2_sel == OP2_IMB      else \
                          imm_u         if self.c_op2_sel == OP2_IMU      else \
                          imm_j         if self.c_op2_sel == OP2_IMJ      else \
                          WORD(0)
        # Control signal to select the next PC
        self.pc_sel     = PC_4


    def update(self):
        opcode2=RISCV.opcode(RR.reg_inst)
        opcode          = RISCV.opcode(self.inst)
        if opcode2==LW and (RR.reg_rd==self.rs1 or (RR.reg_rd==self.rs2 and RR.reg_c_wb_sel!=2)) and RR.reg_rd!=0 and RR.reg_inst!=WORD(BUBBLE) and self.reg_inst!=WORD(BUBBLE) and (opcode not in [ EBREAK, ECALL ]) and opcode!=ILLEGAL:
            RR.reg_inst=WORD(BUBBLE)
            IF.reg_pc=ID.reg_pc
            ID.reg_pc=self.pc

        else:
            RR.reg_inst=self.inst
        RR.reg_c_br_type        =self.c_br_type
        RR.reg_pcplus4          =self.pcplus4
        RR.reg_pc               = self.pc
        RR.reg_c_wb_sel         = self.c_wb_sel
        RR.reg_exception        = self.exception
        RR.reg_rs1              = self.rs1
        RR.reg_rs2              = self.rs2
        RR.reg_rd               = self.rd
        RR.reg_imm              = self.imm
        RR.reg_c_alu_fun        = self.c_alu_fun
        RR.reg_c_rf_wen         = self.c_rf_wen
        RR.reg_c_op1_sel        = self.c_op1_sel
        RR.reg_c_op2_sel        = self.c_op2_sel

        # DO NOT TOUCH -----------------------------------------------
        Pipe.log(S_ID, self.pc, self.inst, self.log())
        #-------------------------------------------------------------


    # DO NOT TOUCH ---------------------------------------------------
    def log(self):
        if self.inst in [ BUBBLE, ILLEGAL ]:
            return('# -')
        else:
            return("# inst=0x%08x, rd=%d rs1=%d rs2=%d imm=0x%08x"
                    % (self.inst, self.rd, self.rs1, self.rs2, self.imm))
    #-----------------------------------------------------------------


#--------------------------------------------------------------------------
#   RR: Register read stage
#--------------------------------------------------------------------------

class RR(Pipe):

    # Pipeline registers ------------------------------
    reg_c_wb_sel        = WORD(0)
    reg_c_br_type       = WORD(0)
    reg_pcplus4         = WORD(0)
    reg_pc              = WORD(0)           # RR.reg_pc
    reg_inst            = WORD(BUBBLE)      # RR.reg_inst
    reg_exception       = WORD(EXC_NONE)    # RR.reg_exception
    reg_rs1             = WORD(0)           # RR.reg_rs1
    reg_rs2             = WORD(0)           # RR.reg_rs2
    reg_rd              = WORD(0)           # RR.reg_rd
    reg_imm             = WORD(0)           # RR.reg_imm
    reg_c_alu_fun       = WORD(ALU_X)       # RR.reg_c_alu_fun
    reg_c_rf_wen        = WORD(REN_0)       # RR.reg_c_rf_wen
    reg_c_op1_sel       = WORD(OP1_X)
    reg_c_op2_sel       = WORD(OP2_X)

    reg_store           = WORD(0)
    #--------------------------------------------------


    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc                 # Pipe.RR.pc
        #   self.inst               # Pipe.RR.inst
        #   self.exception          # Pipe.RR.exception
        #
        #   self.rs1                # Pipe.RR.rs1
        #   self.rs2                # Pipe.RR.rs2
        #   self.rd                 # Pipe.RR.rd
        #   self.imm                # Pipe.RR.imm
        #   self.c_alu_fun          # Pipe.RR.c_alu_fun
        #   self.c_rf_wen           # Pipe.RR.c_rf_wen
        #----------------------------------------------



    def compute(self):
        self.c_wb_sel   = RR.reg_c_wb_sel
        self.c_br_type  = RR.reg_c_br_type
        self.pcplus4    = RR.reg_pcplus4
        self.pc         = RR.reg_pc
        self.inst       = RR.reg_inst
        self.exception  = RR.reg_exception
        self.rs1        = RR.reg_rs1
        self.rs2        = RR.reg_rs2
        self.rd         = RR.reg_rd
        self.c_op1_sel  = RR.reg_c_op1_sel
        self.c_op2_sel  = RR.reg_c_op2_sel
        self.imm        = RR.reg_imm
        self.c_alu_fun  = RR.reg_c_alu_fun
        self.c_rf_wen   = RR.reg_c_rf_wen
        self.store      = RR.reg_store

        # Read register file
        self.op1_data=Pipe.cpu.rf.read(self.rs1)
        self.forjalr=Pipe.cpu.rf.read(self.rs1)
        self.op2_data=Pipe.cpu.rf.read(self.rs2)
        self.store=self.op2_data

        
        if EX.reg_inst!=WORD(BUBBLE) and self.rs1==EX.reg_rd and EX.reg_rd!=0 and EX.reg_c_rf_wen==True:
            self.op1_data=Pipe.EX.alu_out
            self.forjalr=Pipe.EX.alu_out
        elif MM.reg_inst!=WORD(BUBBLE) and self.rs1==MM.reg_rd and MM.reg_rd!=0 and MM.reg_c_rf_wen==True:
            self.op1_data=Pipe.MM.wbdata
            self.forjalr=Pipe.MM.wbdata
        elif Pipe.WB.inst!=WORD(BUBBLE) and self.rs1==Pipe.WB.rd and Pipe.WB.rd!=0 and Pipe.WB.c_rf_wen==True:
            self.op1_data=Pipe.WB.wbdata
            self.forjalr=Pipe.WB.wbdata

        if self.c_op1_sel==OP1_PC:
            self.op1_data=self.pc

        opcode=RISCV.opcode(self.inst)
        if opcode==SW:
            if EX.reg_inst!=WORD(BUBBLE) and self.rs2==EX.reg_rd and EX.reg_rd!=0 and EX.reg_c_rf_wen==True:
                self.store=Pipe.EX.alu_out
            elif MM.reg_inst!=WORD(BUBBLE) and self.rs2==MM.reg_rd and MM.reg_rd!=0 and MM.reg_c_rf_wen==True:
                self.store=Pipe.MM.wbdata
            elif Pipe.WB.inst!=WORD(BUBBLE) and self.rs2==Pipe.WB.rd and Pipe.WB.rd!=0 and Pipe.WB.c_rf_wen==True:
                self.store=Pipe.WB.wbdata
        if (not(opcode==BEQ or opcode==BNE or opcode==BLT or opcode==BGE or opcode==BLTU or opcode==BGEU)) and self.c_op2_sel!=0:
            self.op2_data=self.imm

        else:
            if EX.reg_inst!=WORD(BUBBLE) and self.rs2==EX.reg_rd and EX.reg_rd!=0 and EX.reg_c_rf_wen==True:
                self.op2_data=Pipe.EX.alu_out
            elif MM.reg_inst!=WORD(BUBBLE) and self.rs2==MM.reg_rd and MM.reg_rd!=0 and MM.reg_c_rf_wen==True:
                self.op2_data=Pipe.MM.wbdata
            elif Pipe.WB.inst!=WORD(BUBBLE) and self.rs2==Pipe.WB.rd and Pipe.WB.rd!=0 and Pipe.WB.c_rf_wen==True:
                self.op2_data=Pipe.WB.wbdata

    def update(self):
        EX.reg_c_wb_sel     = self.c_wb_sel
        EX.reg_forjalr      = self.forjalr
        EX.reg_c_br_type    = self.c_br_type
        EX.reg_pcplus4=self.pcplus4
        EX.reg_pc=self.pc
        EX.reg_inst=self.inst
        EX.reg_exception    =self.exception
        EX.reg_rd           =self.rd
        EX.reg_op1_data     =self.op1_data
        EX.reg_op2_data     =self.op2_data
        EX.reg_c_alu_fun    =self.c_alu_fun
        EX.reg_c_rf_wen     =self.c_rf_wen
        EX.reg_c_op1_sel    =self.c_op1_sel
        EX.reg_c_op2_sel    =self.c_op2_sel
        EX.reg_rs1          =self.rs1
        EX.reg_rs2          =self.rs2
        EX.reg_imm          =self.imm
        EX.reg_store        =self.store                                                                                         # DO NOT TOUCH -----------------------------------------------
        Pipe.log(S_RR, self.pc, self.inst, self.log())
        #-------------------------------------------------------------


    # DO NOT TOUCH ---------------------------------------------------
    def log(self):
        if self.inst in [ BUBBLE, ILLEGAL ]:
            return('# -')
        else:
            return("# op1=0x%08x op2=0x%08x" % (self.op1_data, self.op2_data))
    #-----------------------------------------------------------------


#--------------------------------------------------------------------------
#   EX: Execution stage
#--------------------------------------------------------------------------
class EX(Pipe):

    # Pipeline registers ------------------------------
    reg_c_wb_sel        = WORD(0)
    reg_c_br_type       = WORD(0)
    reg_pcplus4         = WORD(0)
    reg_pc              = WORD(0)           # EX.reg_pc
    reg_inst            = WORD(BUBBLE)      # EX.reg_inst
    reg_exception       = WORD(EXC_NONE)    # EX.reg_exception
    reg_rd              = WORD(0)           # EX.reg_rd
    reg_op1_data        = WORD(0)           # EX.reg_op1_data
    reg_op2_data        = WORD(0)           # EX.reg_op2_data
    reg_c_rf_wen        = False             # EX.reg_c_rf_wen
    reg_c_alu_fun       = WORD(ALU_X)       # EX.reg_c_alu_fun
    reg_c_op1_sel       = WORD(0)
    reg_c_op2_sel       = WORD(0)
    reg_rs1             = WORD(0)
    reg_rs2             = WORD(0)
    reg_imm             = WORD(0)
    reg_store           = WORD(0)
    reg_forjalr         = WORD(0)
    #--------------------------------------------------


    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc                 # Pipe.EX.pc
        #   self.inst               # Pipe.EX.inst

    def compute(self):

        # Read out pipeline register values
        self.c_wb_sel           = EX.reg_c_wb_sel
        self.forjalr            = EX.reg_forjalr
        self.c_br_type          = EX.reg_c_br_type
        self.pcplus4            = EX.reg_pcplus4
        self.pc                 = EX.reg_pc
        self.inst               = EX.reg_inst
        self.exception          = EX.reg_exception
        self.rd                 = EX.reg_rd
        self.c_rf_wen           = EX.reg_c_rf_wen
        self.c_alu_fun          = EX.reg_c_alu_fun
        self.op1_data           = EX.reg_op1_data
        self.op2_data           = EX.reg_op2_data
        self.c_op1_sel          = EX.reg_c_op1_sel
        self.c_op2_sel          = EX.reg_c_op2_sel
        self.rs1                = EX.reg_rs1
        self.rs2                = EX.reg_rs2
        self.imm                = EX.reg_imm
        self.store              = EX.reg_store
        # The second input to ALU should be put into self.alu2_data for correct log msg.


        # Perform ALU operation

        self.alu2_data          =self.op2_data

       
        self.alu_out = Pipe.cpu.alu.op(self.c_alu_fun, self.op1_data, self.alu2_data)
        self.forjalr=self.alu_out
        if self.c_wb_sel==2:
            self.alu_out=self.pcplus4
    def update(self):
        opcode= RISCV.opcode(self.inst)
        if self.inst!=WORD(BUBBLE) and (self.c_br_type!=0 and self.c_br_type!=7 and self.c_br_type!=8):
            if self.alu_out==WORD(1):
                IF.reg_pc=self.pcplus4
                EX.reg_inst=WORD(BUBBLE)
                RR.reg_inst=WORD(BUBBLE)
                ID.reg_inst=WORD(BUBBLE)
        elif self.inst!=WORD(BUBBLE) and self.c_br_type==8:
            IF.reg_pc=self.forjalr
            EX.reg_inst=WORD(BUBBLE)
            RR.reg_inst=WORD(BUBBLE)
            ID.reg_inst=WORD(BUBBLE)
        MM.reg_pc               = self.pc
        MM.reg_pcplus4          = self.pcplus4
        MM.reg_exception        = self.exception

        if Pipe.ID.MM_bubble:
            MM.reg_inst         = WORD(BUBBLE)
            MM.reg_c_rf_wen     = False
        else:
            MM.reg_inst         = self.inst
            MM.reg_rd           = self.rd
            MM.reg_c_rf_wen     = self.c_rf_wen
            MM.reg_alu_out      = self.alu_out
            MM.reg_imm          = self.imm
            MM.reg_rs1          = self.rs1
            MM.reg_rs2          = self.rs2
            MM.reg_op1_data     = self.op1_data
            MM.reg_op2_data     = self.op2_data
            MM.reg_store        = self.store                                                                                    # DO NOT TOUCH -----------------------------------------------
        Pipe.log(S_EX, self.pc, self.inst, self.log())
        #-------------------------------------------------------------


    # DO NOT TOUCH ---------------------------------------------------
    def log(self):

        ALU_OPS = {
            ALU_X       : f'# -',
            ALU_ADD     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} + {self.alu2_data:#010x}',
            ALU_SUB     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} - {self.alu2_data:#010x}',
            ALU_AND     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} & {self.alu2_data:#010x}',
            ALU_OR      : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} | {self.alu2_data:#010x}',
            ALU_XOR     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} ^ {self.alu2_data:#010x}',
            ALU_SLT     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} < {self.alu2_data:#010x} (signed)',
            ALU_SLTU    : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} < {self.alu2_data:#010x} (unsigned)',
            ALU_SLL     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} << {self.alu2_data & 0x1f}',
            ALU_SRL     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} >> {self.alu2_data & 0x1f} (logical)',
            ALU_SRA     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} >> {self.alu2_data & 0x1f} (arithmetic)',
            ALU_COPY1   : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} (pass 1)',
            ALU_COPY2   : f'# {self.alu_out:#010x} <- {self.alu2_data:#010x} (pass 2)',
            ALU_SEQ     : f'# {self.alu_out:#010x} <- {self.op1_data:#010x} == {self.alu2_data:#010x}',
        }
        return('# -' if self.inst == BUBBLE else ALU_OPS[self.c_alu_fun]);

class MM(Pipe):

    # Pipeline registers ------------------------------
    reg_pcplus4         = WORD(0)
    reg_pc              = WORD(0)           # MM.reg_pc
    reg_inst            = WORD(BUBBLE)      # MM.reg_inst
    reg_exception       = WORD(EXC_NONE)    # MM.reg_exception
    reg_rd              = WORD(0)           # MM.reg_rd
    reg_c_rf_wen        = False             # MM.reg_c_rf_wen
    reg_alu_out         = WORD(0)           # MM.reg_alu_out
    reg_imm             = WORD(0)
    reg_rs1             = WORD(0)
    reg_rs2             = WORD(0)
    reg_op1_data        = WORD(0)
    reg_op2_data        = WORD(0)
    reg_store           = WORD(0)
    #--------------------------------------------------

    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc                 # Pipe.MW.pc
        #   self.inst               # Pipe.MW.inst
        #   self.exception          # Pipe.MW.exception
        #   self.rd                 # Pipe.MW.rd
        #   self.c_rf_wen           # Pipe.MW.c_rf_wen
        #   self.alu_out            # Pipe.MW.alu_out
    
    def compute(self):

        # Read out pipeline register values
        self.pcplus4        = MM.reg_pcplus4
        self.pc             = MM.reg_pc
        self.inst           = MM.reg_inst
        self.exception      = MM.reg_exception
        self.rd             = MM.reg_rd
        self.c_rf_wen       = MM.reg_c_rf_wen
        self.alu_out        = MM.reg_alu_out
        self.rs1            = MM.reg_rs1
        self.rs2            = MM.reg_rs2
        self.imm            = MM.reg_imm
        self.op1_data       = MM.reg_op1_data
        self.op2_data       = MM.reg_op2_data
        self.store          = MM.reg_store
        # Nothing to do for now
        opcode=RISCV.opcode(self.inst)
        if opcode==SW:

            Pipe.cpu.dmem.access(True, self.alu_out, self.store, M_XWR)

        if opcode==LW:

            self.alu_out=Pipe.cpu.dmem.access(True, self.alu_out, WORD(0), M_XRD)[0]
        self.wbdata         = self.alu_out
        

    def update(self):

        WB.reg_pc           = self.pc
        WB.reg_inst         = self.inst
        WB.reg_exception    = self.exception
        WB.reg_c_rf_wen     = self.c_rf_wen

        WB.reg_wbdata       = self.wbdata
        WB.reg_rd           = self.rd
        # DO NOT TOUCH -----------------------------------------------
        Pipe.log(S_MM, self.pc, self.inst, self.log())
        #-------------------------------------------------------------


    # DO NOT TOUCH ---------------------------------------------------
    def log(self):
        if not self.c_rf_wen:
            return('# -')
        else:
            return('# rd=%d wbdata=0x%08x' % (self.rd, self.wbdata))

class WB(Pipe):

    # Pipeline registers ------------------------------

    reg_pc              = WORD(0)           # WB.reg_pc
    reg_inst            = WORD(BUBBLE)      # WB.reg_inst
    reg_exception       = WORD(EXC_NONE)    # WB.reg_exception
    reg_rd              = WORD(0)           # WB.reg_rd
    reg_c_rf_wen        = False             # WB.reg_c_rf_wen
    reg_wbdata          = WORD(0)           # WB.reg_wbdata

    #--------------------------------------------------

    def __init__(self):
        super().__init__()

        # Internal signals:----------------------------
        #
        #   self.pc                 # Pipe.WB.pc
        #   self.inst               # Pipe.WB.inst
        #   self.exception          # Pipe.WB.exception
        #   self.rd                 # Pipe.WB.rd
        #   self.c_rf_wen           # Pipe.WB.c_rf_wen
        #   self.wbdata             # Pipe.WB.wbdata
        #
        #----------------------------------------------

    def compute(self):
        self.inst=WORD(BUBBLE)
        self.c_rf_wen=False
        self.pc=WORD(0)
        self.exception=WORD(EXC_NONE)
        # Read out pipeline register values
        if WB.reg_inst!=WORD(BUBBLE):
            self.pc             = WB.reg_pc
            self.inst           = WB.reg_inst
            self.exception      = WB.reg_exception
            self.rd             = WB.reg_rd
            self.c_rf_wen       = WB.reg_c_rf_wen
            self.wbdata         = WB.reg_wbdata



        # nothing to compute here


    def update(self):


        if self.inst!=WORD(BUBBLE) and self.c_rf_wen:


            if self.rd!=0:
                Pipe.cpu.rf.write(self.rd, self.wbdata)

        # DO NOT TOUCH -----------------------------------------------
        Pipe.log(S_WB, self.pc, self.inst, self.log())

        if (self.exception):
            return False
        else:
            return True
        # ------------------------------------------------------------


    # DO NOT TOUCH ---------------------------------------------------
    def log(self):
        if self.inst == BUBBLE or (not self.c_rf_wen):
            return('# -')
        else:
            return('# R[%d] <- 0x%08x' % (self.rd, self.wbdata))
    #-----------------------------------------------------------------