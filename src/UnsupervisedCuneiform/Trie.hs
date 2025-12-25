module UnsupervisedCuneiform.Trie ( Path(..)
                                  , PathComp(..)
                                  , Value(..)                                           
                                  , Trie(..)
                                  , TrieR(..)
                                  , MkTrieR(..)
                                  , MatchR(..)
                                  --, Path'(..)
                                  --, type (^:)
                                  , type (^#)
                                  , type (^$)
                                  , type (^*#)
                                  , type (^*##)
                                  , type (^@)
                                  ) where

import GHC.TypeLits (Symbol, Nat)
import GHC.Types (Type)
import GHC.TypeError (TypeError, ErrorMessage(..))
import Torch.Typed (DeviceType, DType)

-- type Configuration = [ ( Symbol, Either [Nat] [Either [Nat] [ [Nat] ] ] ) ]

data PathComp where
  PSym :: Symbol -> PathComp
  PNat :: Nat -> PathComp

-- type family a ^: xs where
--   a ^: '[] = '[]

type Path = [PathComp]

data ModelType :: Type -> Type -> Type

--type family Path' xs where
--  Path' '[] = '[]
  
  
data Value where
  VSym :: Symbol -> Value
  VNat :: Nat -> Value
  VNatList :: [ Nat ] -> Value
  VNatTupleList :: [ ( Nat, Nat ) ] -> Value
  VType :: Type -> Value
  VDevice :: DeviceType -> Nat -> Value
  VDType :: DType -> Value
  VNil :: Value
  VArch :: tp -> spec -> [Path] -> Value --([ Nat ] -> DType -> (DeviceType, Nat) -> Type) -> Value
-- general -> specific



data Trie' where
  Trie' :: PathComp -> Value -> [ Trie' ] -> Trie'

type Trie = [ Trie' ]

type family TrieR (ts :: [ Trie' ]) (ps :: Path) (v :: Value) :: [ Trie' ] where

  -- stop condition for existing path (just change the value)
  TrieR ( ( 'Trie' p _ restV ) ': restH ) (p ': '[] ) v = ( 'Trie' p v restV ) ': restH
  
  -- stop condition for new path (add entry)
  TrieR '[] (p ': '[] ) v = '[ 'Trie' p v '[] ]

  -- recursive condition for existing path
  TrieR ( ( 'Trie' p ov restV ) ': restH ) (p ': restP ) v = ( 'Trie' p ov (TrieR restV restP v) ) ': restH

  -- recursive condition for new path
  TrieR '[] (p ': restP) v = '[ 'Trie' p VNil (TrieR '[] restP v) ]

  -- recursive condition for new path
  TrieR ( t ': restH) (p ': restP) v = t ': (TrieR restH (p ': restP) v)


type family MkTrieR ( kvs :: [ ( Path, Value ) ] ) :: [ Trie' ] where

  MkTrieR '[] = '[]

  MkTrieR ('(p, v) ': rest) = TrieR (MkTrieR rest) p v








type family MatchR ( path :: Path ) ( trs :: [ Trie' ] ) ( best :: Value ) :: Value where

  MatchR _ '[] v = v

  MatchR '[] _ v = v
  --MatchR _ '[] v = (TypeError (Text "Matched value was not a Nat!"))

  -- final path component, match
  MatchR (p ': '[]) ( ( 'Trie' p v _ ) ': _) v = v
  --MatchR (p ': '[]) ( ( 'Trie p v _ ) ': _) _ = (TypeError (Text "Provided default value was not a Nat!"))

  -- non-final path component, match
  MatchR (p ': restP) ( ( 'Trie' p v restV ) ': _) best = MatchR restP restV v

  -- non-final path component, non-match
  MatchR (p ': restP) ( ( 'Trie' q v restV ) ': restH) best = MatchR (p ': restP) restH best


type family ToNat ( v :: Value ) :: Nat where
  ToNat (VNat n) = n
  ToNat _ = (TypeError (Text "Not a Nat!"))

type family ToNatList ( v :: Value ) :: [Nat] where
  ToNatList (VNatList nl) = nl
  ToNatList _ = (TypeError (Text "Not a list of Nats!"))  

type family ToNatTupleList ( v :: Value ) :: [(Nat, Nat)] where
  ToNatTupleList (VNatTupleList ntl) = ntl
  ToNatTupleList _ = (TypeError (Text "Not a list of Nat tuples!"))  

type family ToType ( v :: Value ) :: Type where
  ToType (VType t) = t
  ToType _ = (TypeError (Text "Not a Type!"))  

type family ToSym ( v :: Value ) :: Symbol where
  ToSym (VSym s) = s
  ToSym _ = (TypeError (Text "Not a Symbol!"))  


--type family (^#) ( tr :: Trie ) ( p :: Path ) :: Nat where
--  (^#) tr p = ToNat (MatchR p tr VNil)

type (^#) ( tr :: Trie ) ( p :: Path ) = ToNat (MatchR p tr VNil)
type (^*#) ( tr :: Trie ) ( p :: Path ) = ToNatList (MatchR p tr VNil)
type (^*##) ( tr :: Trie ) ( p :: Path ) = ToNatTupleList (MatchR p tr VNil)
type (^$) ( tr :: Trie ) ( p :: Path ) = ToSym (MatchR p tr VNil)
type (^@) ( tr :: Trie ) ( p :: Path ) = ToType (MatchR p tr VNil)

--type family (^#) ( v :: Value ) :: Nat where  
--  (^#) ( VNat n ) = n

-- type family (^$) ( v :: Value ) :: Symbol where
--   (^$) ( VSym s ) = s


-- type family (^*#) ( v :: Value ) :: [ Nat ] where
--   (^*#) ( VNatList l ) = l
  
-- type family (^*##) ( v :: Value ) :: [ ( Nat, Nat ) ] where
--   (^*##) ( VNatTupleList l ) = l  
  
-- data Path a where
--   PEmpty :: Path '[]
--   PAdd :: n -> Path p -> Path (n ': p)
--   -- = KSym Symbol | KNat Nat

--data ValueType = VSym Symbol | VNat Nat | VType Type


-- type family WrapDict kvs where

-- type family WrapKeyComponent c :: Either Symbol Nat where
--   --WrapKeyComponent (s :: Symbol) = Left s
--   WrapKeyComponent (n :: Nat) = Right n  

-- infixr 5 :>:
-- type a :>: b = (WrapKeyComponent a) ': b
  
--type family WrapKey (cs :: [k]) where
--  WrapKey '[] = '[]
--  WrapKey (c ': rest) = (WrapKeyComponent c) ': (WrapKey rest)




--type family WrapValue v where


--type family MakeConfiguration ( kvs :: [ ( [ Either Symbol Nat ], Either Symbol Nat ) ] ) :: Type

--type family MakeConfiguration kvs where


-- type family Get ( xs :: Configuration ) (k :: Symbol) (depth :: Nat) (entity :: Symbol) fallback where
--   Get ( '( k, (Left v) ) ': rest) k d e f = v
--   Get ( '( nk, v ) ': rest) k d e f = Get rest k d e f
--   Get _ k d e f = (TypeError (Text "No such shape"))


