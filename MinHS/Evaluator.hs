module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Debug.Trace

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           | Funcclos {fname::String, bvar::String, env::VEnv, form::Exp}
           -- Others as needed
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE E.empty e
evaluate bs = evalE E.empty (Let bs (Var "main"))


evalE :: VEnv -> Exp -> Value
evalE g e | trace (show e ++ "\n" ++ show g) False = undefined
evalE g (Con str) = case str of 
        "True" -> B True
        "False"-> B False
        "Nil"->Nil
        _-> error "unimplemented"

evalE g (Num i) = I i

evalE g (App (App (Con "Cons") e)

evalE g (App (App (Prim op) (Num x)) (Num y)) = case op of
        Add->I (x + y)
        Sub->I (x - y)
        Mul->I (x * y)
        Quot->I (quot x y)
        Gt->B (x > y)
        Lt->B (x < y)
        Eq->B (x == y)
        Ne->if (y == 0) then
                error "divide by 0"
            else
                B (x /= y)
        Ge->B (x >= y)
        Le->B (x <= y)

evalE g (App (App (Prim op) (Num x)) e) = 
        evalE g (App (App (Prim op) (Num x)) (Num y))
                where (I y) = evalE g e

evalE g (App (App (Prim op) e1) e2) = 
        evalE g (App (App (Prim op) (Num x)) e2)
                where (I x) = evalE g e1


evalE g (App (Prim op) (Num x)) = case op of
        Neg -> I ((-1) * x)
        _   -> error ("partial application of primops not implemented and never will be")

evalE g (If (e1) (e2) (e3)) = case evalE g e1 of
        B True->evalE g e2
        B False->evalE g e3

evalE g (Var id) = 
        let res = E.lookup g id in
                case res of
                        Nothing->error ("should not happen")
                        Just val->val

evalE g (Let [(Bind bound typ [] (e1))] (e2)) =
        evalE (E.add g (bound, val)) e2
                where val = evalE g e1

evalE g (Letfun (Bind funcname ftype [para] e1)) =
        Funcclos {fname = funcname, env = g, form = e1, bvar = para}        

evalE g (App e1 e2) =
        let e1' = evalE g e1
            e2' = evalE g e2
        in 
                if isFuncclos (e1') then
                        applyFunc g e1' e2'
                else
                        error "unimplemented"
    

isFuncclos:: Value -> Bool
isFuncclos Funcclos {} = True
isFuncclos val = False

applyFunc g (Funcclos {bvar = boundvar, env = fenv, form = fform, fname = name}) val =
    let fc = Funcclos {bvar = boundvar, env = fenv, form = fform, fname = name}
    in evalE (E.addAll fenv [(boundvar, val),(name,fc)]) fform


