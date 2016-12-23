{-# LANGUAGE CPP, TemplateHaskell, QuasiQuotes #-}
module Data.Vinyl.TH where
import Language.Haskell.TH
import Language.Haskell.TH.Quote
-- import Debug.Trace

deal :: QuasiQuoter
deal = QuasiQuoter (\x -> dealWithIt (mkName x)) undefined undefined undefined

dealWithIt :: Name -> Q Exp
dealWithIt n =
  do i <- reify n
     let ty = case i of
#if MIN_VERSION_template_haskell(2,11,0)
                VarI _ t _ -> t
#else
                VarI _ t _ _ -> t
#endif
                _ -> error "I can only fix the type of a value variable"
     [e| ($(varE n) :: $(pure $ fixEither ty)) |]

fixEither :: Type -> Type
-- fixEither t | trace ("fixEither: "++show t) False = undefined
fixEither (ForallT _ ctx (AppT (AppT e (AppT (AppT record f) _)) r)) =
  AppT (AppT e l') r
  where l' = AppT (AppT record f) (constraintsToType ctx)

-- | Pick out the @t@ from whatever @RElem t ts (RIndex t ts)@
-- constraints there are in the context of a quantified type.
constraintsToType :: [Type] -> Type
constraintsToType ctx = promotedListT (foldMap aux ctx)
  where aux (AppT (AppT _elem t) _) = [t]
        aux (AppT (AppT (AppT _elem t) _) _) = [t]
        aux _ = []

-- tr :: Show a => a -> a
-- tr x = trace (show x) x

promotedListT :: [Type] -> Type
promotedListT [] = PromotedNilT
promotedListT (t : ts) = AppT (AppT PromotedConsT t) (promotedListT ts)
