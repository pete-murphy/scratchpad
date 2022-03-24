From `fp-ts/lib/Ord`

```ts
const contramap: <A, B>(f: (b: B) => A) => (fa: Ord<A>) => Ord<B>

// where
interface Ord<A> extends Eq<A> {
  compare: (x: A, y: A) => Ordering
}
```
