module Tasks where

-- tasks are either completed or not
data Tasks = Incomplete (Maybe Priority) (Maybe Date) [Project] [Context] String
     | Completed Date Tasks


-- tasks reference metadata tokens
type Priority = Char -- (A) is high, (B) is lower, ...
type Project = String -- +<proj name>
type Context = String -- @<context>

data Date = Date Int Int Int

instance Eq Date where
    (Date m1 d1 y1) == (Date m2 d2 y2) = m1 == m2 
        && d1 == d2 && y1 == y2

instance Ord Date where
    compare (Date m1 d1 y1) (Date m2 d2 y2) =
        if y1 /= y2
        then compare y1 y2
        else if m1 /= m2
             then compare m1 m2
             else compare d1 d2

-- pretty printing
instance Show Date where
    show (Date month day year) = show month
        ++ "-" ++ show day
        ++ "-" ++ show year

instance Show Tasks where
    show (Completed date task) = "x " ++ (show date) ++ " " ++ (show task)
    show (Incomplete mPriority mDate _ _ str) =
            (showPriority mPriority) 
            ++ (showDate mDate)
            ++ str
        where showPriority (Just p) = "(" ++ [p] ++ ") "
              showPriority Nothing = ""
              showDate (Just d) = show d ++ " "
              showDate Nothing = ""






