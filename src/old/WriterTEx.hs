module WriterTEx where

import Control.Monad.Trans.Writer

-- import { getWriterM } from 'fp-ts/lib/WriterT';
-- import * as T from 'fp-ts/lib/Task';
-- const WT = getWriterM(T.task);
-- const program = pipe(
--     WT.fromIO(() => 100),
--     WT.tell('start'),
--     WT.map((x) => x * 2)
--     WT.tell('double'),
-- );
-- program() // [200, ['start', 'double']]

(&) :: a -> (a -> c) -> c
(&) = flip ($)

program :: WriterT [String] IO Int
program =
  pure 100
    & (>>) (tell ["start"])
    & fmap (* 2)
    & (>>) (tell ["double"])
