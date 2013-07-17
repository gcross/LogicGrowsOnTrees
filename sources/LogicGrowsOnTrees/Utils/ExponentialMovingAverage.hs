module ExponentialMovingAverage

data ExponentialMovingAverage α = ExponentialMovingAverage α
    { factor :: !α
    , value  :: !α
    } deriving (Eq,Show)

initialEMA :: Num α ⇒ α → ExponentialMovingAverage α → ExponentialMovingAverage α
initialEMA datapoint (ExponentialMovingAverage factor old_value) = (ExponentialMovingAverage factor ((1-α)*datapoint + α*old_value))

updateEMAValue :: Num α ⇒ α → ExponentialMovingAverage α → ExponentialMovingAverage α
updateEMAValue datapoint (ExponentialMovingAverage factor old_value) = (ExponentialMovingAverage factor ((1-α)*datapoint + α*old_value))
