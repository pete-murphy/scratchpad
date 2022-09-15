type State s a = s -> (a, s)

-- <S1, S2 extends S1>(f: (s1: S1) => S2) => <A>(fa: State<S1, A>) => State<S2, A>
mapS :: (s1 -> s2) -> State s1 a -> State s2 a
mapS f state1 = \s2 -> ()
