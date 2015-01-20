;methodPass.yml

(CoreServices.Begin.begin 
    '(
        (temp.VarArrayElem tests (ExpLit 0).m1 temp.VarIdent test.m2) 
        (temp.VarIdent test.m1 temp.VarArrayElem tests (ExpLit 0).m2) 
        (CoreServices.RETURN.return '0)))