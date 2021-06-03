# pipeline
This is an implementation of 'pipeline' using python

### dealing with data hazard    
Data hazards occur when it is needed to read the register value when the former stages try to write new values to the register. Dependencies occur since new value should be ready before computing the ALU operation but without forwarding data is written at the end of the WB stage. Data hazards can occur with the previous instruction, or instruction at two stages ahead, or instruction at three stages ahead. Thus, we need three forwarding logic which are forwarding from EX stage to RR stage, MM stage to RR stage, and WB stage to RR stage, requiring alu_out data or wbdata. Control signal should detect whether there is dependency with previous stage and should select the right rs1_data value or rs2_data value using mux. 

    if EX.reg_inst!=WORD(BUBBLE) and self.rs1==EX.reg_rd and EX.reg_rd!=0 and EX.reg_c_rf_wen==True:

    elif MM.reg_inst!=WORD(BUBBLE) and self.rs1==MM.reg_rd and MM.reg_rd!=0 and MM.reg_c_rf_wen==True:

    elif Pipe.WB.inst!=WORD(BUBBLE) and self.rs1==Pipe.WB.rd and Pipe.WB.rd!=0 and Pipe.WB.c_rf_wen==True:    
    
These are the three conditions for rs1 value having dependency with previous instructions. The order should be as the above code since most recent data should be forwarded. 
Forwarding should be detected not only for op1_data and op2_data but should be done for rs2_data since store opcode requires the value of rs2 other than op2_data(immediate value in this case) and rs2_data also has a possibility of having dependency with the previous logic. Thus, control signal should also detect this case additionally. 
Load use hazard also occurs. This occurs because the output value of the load is determined after the MM stage but we have to forward the logic in the RR stage. Thus in this case, forwarding alone does not solve the data hazards and we have to stall the pipeline. Thus, control signal detects load use hazard in the ID stage and stalls IF stage and ID stage and make it a bubble after ID stage. In this way, we can properly have the forwarded data after one cycle stall.  

### how to resolve data hazard   
After reading rs1 and rs2 through register file, we have to decide whether certain instruction needs forwarding or not. Thus, it is required to check certain conditions (as is indicated in the above code) and gives proper forwarding signal. Since the most recent data should be forwarded, the conditions should be checked from EX stage and if forwarding condition is met, you don’t have to check previous stages before EX stage. In other words, this can be all done by using 3 MUXes (for op1_data, op2_data, rs2_data(in case of store instruction)) and forwarding signal from control signal. 


### possible control hazards with always-taken branch
Control hazards can occur during branch instruction. This is because we have to wait until branch outcome is determined before fetching the next instruction. When c_br_type is not 0(except jal, jalr instruction) , control hazards are likely to occur since we have to stall the pipeline until the result is determined. Thus, we use always-taken branch instruction scheme. At the IF stage, we calculate the target branch address, assuming the branch is taken. However, at the end of the EX stage, when the outcome is determined, there are chances that branch is actually not taken, which is different from the first assumption. Thus, in that case, we have to make the instructions that were following as all bubbles, and fetch the correct instruction, which is the value of pcplus4 of the EX stage. If branch is predicted correctly, pipeline is not stalled but if branch is predicted incorrectly, which means that the result was “not taken”, three cycles are wasted.  
In the case of jal instruction, there is no chance we might mispredict so putting the branch address adder in the IF stage can all solve the problem.
However, in the case of jalr instruction, if we calculate the target address in the IF stage, that might make our problem more complicated. This is because, jalr calculates the target address with one register value, but this value might have dependency. Therefore, if one would calculate jalr branch address in the IF stage, that means we need additional forwarding logic. To make problems simple, we use “always-not-taken” assumption for jalr and forward the right PC after calculating it at the EX stage. 


### how to resolve control hazards
Thus additional branch target address adder should be added in the IF stage in order to compute the branch target address and fetch the instruction right after that. Additionally, candidates for the next PC should be able to encompass every possible situation since it can also be pcplus4 at the EX stage due to mispredicted branch, or it can also be the proper address of the jalr instruction. Since next PC value could also be the branch target address calculated in the IF stage, there is one more candidate for next PC compared to snurisc5. Thus one input should be added to the mux used in the IF stage. 
