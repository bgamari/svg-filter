module StringMatch
    ( StringMatch(..)
    , renderStringMatch
    , litString
    , matches
    ) where

data StringMatch = LitChar Char StringMatch
                 | Wildcard StringMatch
                 | WildcardStar StringMatch
                 | EndMatch
                 deriving (Eq, Ord)

instance Show StringMatch where
    showsPrec _ = renderStringMatch

renderStringMatch :: StringMatch -> ShowS
renderStringMatch x = showChar '"' . go x . showChar '"'
  where
    go (LitChar c rest) = showChar c . go rest
    go (Wildcard rest) = showChar '?' . go rest
    go (WildcardStar rest) = showChar '*' . go rest
    go EndMatch = id

instance Monoid StringMatch where
    mempty = EndMatch

instance Semigroup StringMatch where
    LitChar c rest <> x = LitChar c (rest <> x)
    Wildcard rest <> x = Wildcard (rest <> x)
    WildcardStar rest <> x = WildcardStar (rest <> x)
    EndMatch <> x = x

litString :: String -> StringMatch
litString = foldr LitChar EndMatch

matches :: StringMatch -> String -> Bool
matches (LitChar c rest) (x:xs) = c == x && matches rest xs
matches (Wildcard rest) (x:xs) = matches rest xs
matches (WildcardStar rest) (x:xs) = matches rest (x:xs) || matches (WildcardStar rest) xs
matches (WildcardStar rest) [] = matches rest []
matches EndMatch [] = True
matches EndMatch (_:_)  = False
matches _ [] = False

