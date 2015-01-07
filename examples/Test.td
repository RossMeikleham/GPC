;Test.yml

(CoreServices.Begin.begin 
    '(Kernel.Multi.Multi '20 '20) 
    '(CoreServices.Seq.seq 
        (CoreServices.reg.write '1 
            (CoreServices.RETURN.return '6)) 
        (CoreServices.IF.if 
            (CoreServices.ALU.lt 
                (CoreServices.Reg.read '1) '3) 
            (CoreServices.RETURN.return 
                (CoreServices.ALU.plus 
                    (
                        (CoreServices.reg.write '2 temp.VarIdent test.print) 
                        (
                            (temp.VarIdent test.print '0) 
                            (temp.VarIdent test.print '1) 
                            (temp.VarIdent test.print '2) 
                            (temp.VarIdent test.print '3) 
                            (temp.VarIdent test.print '4) 
                            (temp.VarIdent test.print '5)) 
                        (CoreServices.reg.write '3 temp.VarIdent test.m1) 
                        (CoreServices.RETURN.return '1)) '48)) 
            (temp.VarArrayElem multi (ExpLit 0).m2 
                (CoreServices.RETURN.return '6)))))