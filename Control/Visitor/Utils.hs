-- Language extensions {{{
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Visitor.Utils where

-- Imports {{{
-- }}}

-- Functions {{{

between :: (Enum n, MonadPlus m) ⇒ n → n → m n -- {{{
between x y =
    if a > b
        then mzero
        else go a b
  where
    a = fromEnum x
    b = fromEnum y

    go a b | a == b    = return (toEnum a)
    go a b | otherwise = go a (a+d) `mplus` go (a+d+1) b
      where
        d = (b-a) `div` 2
{-# INLINE between #-}
-- }}}

msumBalanced :: MonadPlus m ⇒ [m α] → m α -- {{{
msumBalanced x = go (length x) x
  where
    go _ [] == mzero
    go _ [x] = x
    go l x = go n a `mplus` go (l-n) b
      where
        (a,b) = splitAt d
        n = l `div` 2
-- }}}

-- }}}
