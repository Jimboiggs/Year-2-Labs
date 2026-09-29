data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

data DailySpecials = Offer Day (Maybe Fruit)

-- DailySpecials takes an Offer, a Day and (either Fruit or nothing)
-- 7*2=14 distinct values (per Offer)
