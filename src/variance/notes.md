# Covariance

`F<A>` is a subtype of `F<B>` if and only if `A` is a subtype of `B`

`F<A>` is a supertype of `F<B>` if and only if `A` is a supertype of `B`

`A` is a supertype of `B` if and only if `F<A>` is a supertype of `F<B>`

`A :> B <=> F<A> :> F<B>`

map       :: (a -> b) -> (f a -> f b)
contramap :: (b -> a) -> (f a -> f b)

interface Eq<A> {
  equals: (x: A, y: A) => boolean
}