;Test3.yml

(CoreServices.Begin.begin 
    '(GPRM.Kernel.Test.Test '0) 
    '(GPRM.Kernel.Test.Test '1) 
    '(
        (CoreServices.reg.write '4 
            (CoreServices.ALU.plus 
                (CoreServices.Reg.read '1) '6)) 
        (CoreServices.reg.write '5 
            (CoreServices.ALU.minus 
                (CoreServices.Reg.read '2) '7)) 
        (CoreServices.reg.write '6 temp.VarArrayElem tests (ExpLit 0).m0) 
        (
            (temp.VarArrayElem tests (ExpIdent i).m1 
                (CoreServices.Reg.read '1) 
                (CoreServices.Reg.read '2) 
                (CoreServices.Reg.read '3) 
                (CoreServices.ALU.plus 
                    (CoreServices.Reg.read '6) '5)) 
            (temp.VarArrayElem tests (ExpIdent i).m1 
                (CoreServices.Reg.read '1) 
                (CoreServices.Reg.read '2) 
                (CoreServices.Reg.read '3) 
                (CoreServices.ALU.plus 
                    (CoreServices.Reg.read '6) '5))) 
        (CoreServices.RETURN.return '0)))