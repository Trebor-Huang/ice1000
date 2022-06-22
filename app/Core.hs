module Core where
import Utils

-- | The core language
data Ice10
  = Var !Name
  | Con !Name ![Ice10] -- ^ Constructors
  | Cut !Name ![Ice10]
    -- ^ Functions, fully applied.
    -- Defined functions / Builtin functions.
  | Eff !Name ![Ice10] !(Scoped Ice10)
    -- ^ Builtin Effects
  | Atom !Atomic
    -- ^ Basic datatypes
  | Case !(Maybe Ice10) ![(Name, Scoped Ice10)] !(Maybe (Scoped Ice10))
    -- ^ Case distinctions either on the currently focused continuation
    -- or on a term, @(Name, scope)@ gives a constructor label, and binds
    -- some pattern variables. The last @Maybe scope@ is for fallthrough cases.
  | Prog !Ice10 !Ice10
    -- ^ The former @term@ must be defined by patterns, and the latter defined
    -- by constructors.
  deriving (Show, Eq)

class MonadFail m => Ice10Eff m where
  handle
    :: Name  -- ^ Effect name
    -> [Ice10]  -- ^ Arguments
    -> Scoped Ice10  -- ^ Continuation Term
    -> (Ice10 -> m Ice10)  -- ^ Inverted control
    -> m Ice10


