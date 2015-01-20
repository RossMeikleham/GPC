;arithmetic.yml

(CoreServices.Begin.begin 'GPRM.Kernel.Test.Test 
    '(
        (CoreServices.reg.write '1 
            (CoreServices.ALU.shl '4 '3)) 
        (CoreServices.reg.write '2 
            (CoreServices.ALU.eq 
                (CoreServices.Reg.read '1) 
                (CoreServices.Reg.read '1))) 
        (CoreServices.reg.write '3 '5.03) 
        (CoreServices.reg.write '4 ''c') 
        (CoreServices.reg.write '5 'test) 
        (CoreServices.reg.write '6 
            (CoreServices.ALU.bor 
                (CoreServices.ALU.minus 
                    (CoreServices.ALU.plus 
                        (CoreServices.Reg.read '1) 
                        (CoreServices.Reg.read '1)) 
                    (CoreServices.ALU.over 
                        (CoreServices.ALU.times 
                            (CoreServices.Reg.read '1) 
                            (CoreServices.Reg.read '1)) 
                        (CoreServices.Reg.read '1))) 
                (CoreServices.ALU.bxor 
                    (CoreServices.ALU.band 
                        (CoreServices.Reg.read '1) 
                        (CoreServices.Reg.read '1)) 
                    (CoreServices.ALU.shr 
                        (CoreServices.Reg.read '1) '2)))) 
        (CoreServices.reg.write '7 
            (CoreServices.ALU.or 
                (CoreServices.ALU.and 
                    (CoreServices.ALU.lteq 
                        (CoreServices.Reg.read '6) '5) 
                    (CoreServices.ALU.gteq 
                        (CoreServices.Reg.read '6) '5)) 
                (CoreServices.ALU.neq 
                    (CoreServices.Reg.read '6) '2))) 
        (CoreServices.reg.write '8 
            (CoreServices.ALU.neq 
                (CoreServices.Reg.read '6) 
                (CoreServices.Reg.read '6))) 
        (CoreServices.reg.write '9 
            (CoreServices.ALU.lt '5 '4)) 
        (CoreServices.reg.write '10 
            (CoreServices.ALU.gt '5 '4)) 
        (CoreServices.reg.write '11 
            (CoreServices.ALU.lteq '5 '4)) 
        (CoreServices.reg.write '12 
            (CoreServices.ALU.gteq '5 '4)) 
        (CoreServices.reg.write '13 
            (CoreSevices.ALU.bnot '5)) 
        (CoreServices.reg.write '14 
            (CoreSevices.ALU.not 'True)) 
        (CoreServices.reg.write '15 
            (CoreSevices.ALU.neg '6)) 
        (CoreServices.reg.write '16 
            (CoreServices.ALU.plus '0.5 '0.5)) 
        (CoreServices.reg.write '17 
            (CoreServices.ALU.plus 
                (CoreServices.Reg.read '16) 
                (CoreServices.Reg.read '3))) 
        (CoreServices.reg.write '18 
            (CoreServices.ALU.plus 
                (CoreServices.Reg.read '13) '10)) 
        (CoreServices.reg.write '19 
            (CoreServices.ALU.plus 
                (CoreServices.Reg.read '13) 
                (CoreServices.Reg.read '18))) 
        (CoreServices.reg.write '20 
            (CoreServices.ALU.eq 
                (CoreServices.Reg.read '14) 'True)) 
        (CoreServices.reg.write '21 
            (CoreServices.ALU.and 
                (CoreServices.ALU.eq 
                    (CoreServices.Reg.read '20) 
                    (CoreServices.Reg.read '14)) 'True)) 
        (CoreServices.reg.write '22 
            (CoreServices.ALU.or 
                (CoreServices.Reg.read '21) 'False)) 
        (CoreServices.reg.write '23 
            (CoreServices.ALU.neq 
                (CoreServices.Reg.read '22) 
                (CoreServices.Reg.read '21))) 
        (CoreServices.reg.write '24 
            (CoreServices.ALU.and 
                (CoreServices.Reg.read '23) 
                (CoreServices.Reg.read '22))) 
        (CoreServices.reg.write '25 '0.5) 
        (CoreServices.reg.write '26 
            (CoreSevices.ALU.neg '0.5)) 
        (CoreServices.reg.write '27 temp.VarIdent obj.m1) 
        (CoreServices.reg.write '28 
            (CoreServices.ALU.mod 
                (CoreServices.Reg.read '15) 
                (CoreServices.Reg.read '13))) 
        (CoreServices.RETURN.return '1)))