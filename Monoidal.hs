module Monoidal where

class Functor f => Monoidal f where
    unit  :: f ()                   -- Identity
    (<^>) :: f a -> f b -> f (a, b) -- Tensor product

-- Applicative's (<*>)
(<@>) :: Monoidal f => f (a -> b) -> f a -> f b
(<@>) ff fa = uncurry ($) <$> ff <^> fa

-- Monoidal instance for the arrow functor, allowing (<@>) to behave like `f a b c = a c (b c)`.
instance Monoidal ((->) r) where
    unit :: r -> ()
    unit r = ()

    (<^>) :: (r -> a) -> (r -> b) -> r -> (a, b)
    (<^>) ra rb r = ((ra r), (rb r))
 -- (<^>) Could be defined as (&&&) from Control.Arrow, as it is the tensor product in the arrow category.
