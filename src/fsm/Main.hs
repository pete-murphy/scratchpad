{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module EnforcingLegalStateTransitions where

import Checkout
  ( Card (..),
    CartItem,
    OrderId (..),
    calculatePrice,
    mkItem,
    newOrderId,
  )
import qualified ConsoleInput
import Control.Monad.IO.Class
import Data.List.NonEmpty
import Data.Semigroup
import qualified Data.Text.IO as T
import qualified PaymentProvider

data NoItems

data HasItems

data NoCard

data CardSelected

data CardConfirmed

data OrderPlaced

class Checkout m where
  type State m :: * -> *
  initial ::
    m (State m NoItems)
  select ::
    SelectState m ->
    CartItem ->
    m (State m HasItems)
  clearCart ::
    ClearState m ->
    m (State m NoItems)
  checkout ::
    State m HasItems ->
    m (State m NoCard)
  selectCard ::
    State m NoCard ->
    Card ->
    m (State m CardSelected)
  confirm ::
    State m CardSelected ->
    m (State m CardConfirmed)
  placeOrder ::
    State m CardConfirmed ->
    m (State m OrderPlaced)
  cancel ::
    CancelState m ->
    m (State m HasItems)
  end ::
    State m OrderPlaced ->
    m OrderId

data SelectState m
  = NoItemsSelect (State m NoItems)
  | HasItemsSelect (State m HasItems)

data ClearState m
  = HasItemsClear (State m HasItems)
  | NoCardClear (State m NoCard)
  | CardSelectedClear (State m CardSelected)
  | CardConfirmedClear (State m CardConfirmed)

data CancelState m
  = NoCardCancel (State m NoCard)
  | CardSelectedCancel (State m CardSelected)
  | CardConfirmedCancel (State m CardConfirmed)

fillCart ::
  (Checkout m, MonadIO m) =>
  State m NoItems ->
  m (State m HasItems)
fillCart noItems = do
  item <- ConsoleInput.prompt "First item:"
  hasItems <- select (NoItemsSelect noItems) (mkItem item)
  selectMoreItems hasItems

selectMoreItems ::
  (Checkout m, MonadIO m) =>
  State m HasItems ->
  m (State m HasItems)
selectMoreItems s = do
  shouldClear <- ConsoleInput.confirm "Clear cart?"
  more <- ConsoleInput.confirm "More items?"
  if shouldClear
    then clearCart (HasItemsClear s) >>= fillCart
    else
      if more
        then
          mkItem <$> ConsoleInput.prompt "Next item:"
            >>= select (HasItemsSelect s)
            >>= selectMoreItems
        else return s

startCheckout ::
  (Checkout m, MonadIO m) =>
  State m HasItems ->
  m (State m OrderPlaced)
startCheckout hasItems = do
  noCard <- checkout hasItems
  card <- ConsoleInput.prompt "Card:"
  cardSelected <- selectCard noCard (Card card)
  useCard <- ConsoleInput.confirm ("Confirm use of '" <> card <> "'?")
  if useCard
    then confirm cardSelected >>= placeOrder
    else
      cancel (CardSelectedCancel cardSelected)
        >>= selectMoreItems
        >>= startCheckout

checkoutProgram ::
  (Checkout m, MonadIO m) =>
  m OrderId
checkoutProgram =
  initial >>= fillCart >>= startCheckout >>= end

newtype CheckoutT m a
  = CheckoutT
      { runCheckoutT :: m a
      }
  deriving
    ( Functor,
      Monad,
      Applicative,
      MonadIO
    )

data CheckoutState s where
  NoItems ::
    CheckoutState
      NoItems
  HasItems ::
    NonEmpty CartItem ->
    CheckoutState HasItems
  NoCard ::
    NonEmpty CartItem ->
    CheckoutState NoCard
  CardSelected ::
    NonEmpty CartItem ->
    Card ->
    CheckoutState CardSelected
  CardConfirmed ::
    NonEmpty CartItem ->
    Card ->
    CheckoutState CardConfirmed
  OrderPlaced ::
    OrderId ->
    CheckoutState OrderPlaced

instance (MonadIO m) => Checkout (CheckoutT m) where
  type State (CheckoutT m) = CheckoutState
  initial = return NoItems
  select state item =
    case state of
      NoItemsSelect NoItems ->
        return (HasItems (item :| []))
      HasItemsSelect (HasItems items) ->
        return (HasItems (item <| items))
  clearCart clearState = pure NoItems

  checkout (HasItems items) =
    return (NoCard items)

  selectCard (NoCard items) card =
    return (CardSelected items card)

  confirm (CardSelected items card) =
    return (CardConfirmed items card)
  placeOrder (CardConfirmed items card) = do
    orderId <- newOrderId
    let price = calculatePrice items
    PaymentProvider.chargeCard card price
    return (OrderPlaced orderId)
  cancel cancelState =
    case cancelState of
      NoCardCancel (NoCard items) ->
        return (HasItems items)
      CardSelectedCancel (CardSelected items _) ->
        return (HasItems items)
      CardConfirmedCancel (CardConfirmed items _) ->
        return (HasItems items)
  end (OrderPlaced orderId) = return orderId

example :: IO ()
example = do
  OrderId orderId <- runCheckoutT checkoutProgram
  T.putStrLn ("Completed with order ID: " <> orderId)
