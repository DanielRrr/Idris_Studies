module Demo

data Ty = TyNat | TyBool | TyString

evalType : Ty -> Type
evalType TyNat = Nat
evalType TyBool = Bool
evalType TyString = String

initVal : (ty : Ty) -> evalType ty
initVal TyNat = Z
initVal TyBool = False
initVal TyString = ""

toString : (ty : Ty) -> evalType ty -> String
toString TyNat x = show x ++ " : Nat"
toString TyBool x = show x ++ " : Bool"
toString TyString x = show x ++ " : String"
