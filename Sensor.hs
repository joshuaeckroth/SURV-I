module Sensor where

class Sensor a where
    getNext :: (SensorData b) => a -> [b]

class SensorData a where
    getSensor :: (Sensor b) => a -> b

