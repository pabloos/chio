

{-
    Errors.hs defines the semantic error domain
-}

module Frontend.Semantic.Error.Errors where

data Error =  NotEmptyParams
            | NoMain
            | TypedMain
            | NotEmpTyMainReturn
            | ReturnNeeded
            | ReturnTypeMissmatch
            | TooManyArguments
            | TooFewArguments
            | InvalidArgType String 
            | UnknownFunction String
            | TypeMissmatch
            | SymbolAlreadyDefined
            | SymbolNotFound String
            | SymbolNotVariable String
            | SymbolNotFunction String
            deriving (Eq, Ord)

instance Show Error where
    -- main function errors
    show NotEmptyParams     = "main function shouldn't have params"
    show NoMain             = "main function not present"
    show TypedMain          = "main function shouldn't have return type annotations"
    show NotEmpTyMainReturn = "main function should have only empty returns"

    -- symbols
    show SymbolAlreadyDefined       = "this symbol has already been defined or it's not accesible from its scope"
    show (SymbolNotFound name)      = show name ++ " symbol not found. You have to declare it previously"
    show (SymbolNotVariable name)   = show name ++ " isn't a variable"
    show (SymbolNotFunction name)   = show name ++ " isn't a function"

    -- return
    show ReturnNeeded        = "no return found"
    show ReturnTypeMissmatch = "return type and function type annotation does not match"

    -- calls
    show TooFewArguments        = "not enough arguments"
    show TooManyArguments       = "too many arguments"
    show (InvalidArgType name)  = show name ++ "this argument doesn't match the param type"
    show (UnknownFunction name) = show name ++ "function callee not found"

    show TypeMissmatch = "types doesn't match"