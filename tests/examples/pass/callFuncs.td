;callFuncs.yml

(CoreServices.Begin.begin 'GPRM.Kernel.Test.Test 
    '(
        (CoreServices.reg.write '12 
            (
                (CoreServices.reg.write '7 
                    (
                        (CoreServices.reg.write '3 '4) temp.VarIdent test.m1 
                        (CoreServices.IF.if 
                            (CoreServices.ALU.lt 
                                (CoreServices.Reg.read '3) '4) 
                            (CoreServices.RETURN.return 
                                (CoreServices.ALU.eq '2 '3)) 
                            (
                                (
                                    (CoreServices.IF.if 
                                        (CoreServices.ALU.lt '0 '3) 
                                        (CoreServices.reg.write '4 '5) '0) CoreServices.Seq.seq) 
                                (
                                    (CoreServices.IF.if 
                                        (CoreServices.ALU.lt '1 '3) 
                                        (CoreServices.reg.write '5 '5) '0) CoreServices.Seq.seq) 
                                (
                                    (CoreServices.IF.if 
                                        (CoreServices.ALU.lt '2 '3) 
                                        (CoreServices.reg.write '6 '5) '0) CoreServices.Seq.seq))) 
                        (CoreServices.RETURN.return 
                            (CoreServices.ALU.eq '2 '3)))) 
                (
                    (CoreServices.reg.write '8 '4) temp.VarIdent test.m1 
                    (CoreServices.IF.if 
                        (CoreServices.ALU.lt 
                            (CoreServices.Reg.read '8) '4) 
                        (CoreServices.RETURN.return 
                            (CoreServices.ALU.eq 
                                (CoreSevices.ALU.neg '3) 
                                (CoreSevices.ALU.bnot '5))) 
                        (
                            (
                                (CoreServices.IF.if 
                                    (CoreServices.ALU.lt '0 '3) 
                                    (CoreServices.reg.write '9 '5) '0) CoreServices.Seq.seq) 
                            (
                                (CoreServices.IF.if 
                                    (CoreServices.ALU.lt '1 '3) 
                                    (CoreServices.reg.write '10 '5) '0) CoreServices.Seq.seq) 
                            (
                                (CoreServices.IF.if 
                                    (CoreServices.ALU.lt '2 '3) 
                                    (CoreServices.reg.write '11 '5) '0) CoreServices.Seq.seq))) 
                    (CoreServices.RETURN.return 
                        (CoreServices.ALU.eq 
                            (CoreSevices.ALU.neg '3) 
                            (CoreSevices.ALU.bnot '5)))) 
                (CoreServices.RETURN.return '2))) 
        (CoreServices.RETURN.return '5) 
        (CoreServices.RETURN.return 
            (
                (CoreServices.reg.write '13 '4) temp.VarIdent test.m1 
                (CoreServices.IF.if 
                    (CoreServices.ALU.lt 
                        (CoreServices.Reg.read '13) '4) 
                    (CoreServices.RETURN.return 
                        (CoreServices.ALU.eq '20 
                            (CoreServices.Reg.read '12))) 
                    (
                        (
                            (CoreServices.IF.if 
                                (CoreServices.ALU.lt '0 '3) 
                                (CoreServices.reg.write '14 '5) '0) CoreServices.Seq.seq) 
                        (
                            (CoreServices.IF.if 
                                (CoreServices.ALU.lt '1 '3) 
                                (CoreServices.reg.write '15 '5) '0) CoreServices.Seq.seq) 
                        (
                            (CoreServices.IF.if 
                                (CoreServices.ALU.lt '2 '3) 
                                (CoreServices.reg.write '16 '5) '0) CoreServices.Seq.seq))) 
                (CoreServices.RETURN.return 
                    (CoreServices.ALU.eq '20 
                        (CoreServices.Reg.read '12)))))))