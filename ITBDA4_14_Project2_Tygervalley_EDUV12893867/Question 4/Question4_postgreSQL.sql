\set my_place 'Cape Town CBD'
\set radius_km '0'
\set my_category 'All'

DROP TABLE IF EXISTS events;
DROP TABLE IF EXISTS places;

CREATE TABLE places (
    place VARCHAR(40) PRIMARY KEY,
    latitude NUMERIC(8, 5) NOT NULL,
    longitude NUMERIC(8, 5) NOT NULL
);

CREATE TABLE events (
    event_id INTEGER PRIMARY KEY,
    source VARCHAR(40) NOT NULL,
    event_name TEXT NOT NULL,
    category VARCHAR(30) NOT NULL,
    event_type  VARCHAR(15) NOT NULL,          
    start_date DATE NOT NULL,
    end_date  DATE,
    start_time  TIME,
    price_rand  NUMERIC(8, 2),
    is_free BOOLEAN,
    price_imputed  BOOLEAN,
    venue  TEXT,
    place  VARCHAR(40) REFERENCES places (place),
    latitude  NUMERIC(8, 5),
    longitude  NUMERIC(8, 5),
    location_imputed BOOLEAN,
    summary  TEXT,
    url TEXT
);

\copy places FROM 'C:/Users/ralle/Desktop/HNR/ITBDA4-14/Project 2/Question 4/places.csv' WITH (FORMAT csv, HEADER true)
\copy events FROM 'C:/Users/ralle/Desktop/HNR/ITBDA4-14/Project 2/Question 4/events_clean.csv' WITH (FORMAT csv, HEADER true, ENCODING 'UTF8')

SELECT COUNT(*) AS events_loaded FROM events;

CREATE OR REPLACE FUNCTION distance_km(lat1 FLOAT, lon1 FLOAT, lat2 FLOAT, lon2 FLOAT)
RETURNS NUMERIC AS $$
    SELECT ROUND((6371 * 2 * ASIN(SQRT(
               POWER(SIN(RADIANS(lat2 - lat1) / 2), 2) +
               COS(RADIANS(lat1)) * COS(RADIANS(lat2)) * POWER(SIN(RADIANS(lon2 - lon1) / 2), 2)
           )))::NUMERIC, 1);
$$ LANGUAGE sql IMMUTABLE;

DROP TABLE IF EXISTS near_me;

CREATE TEMP TABLE near_me AS
SELECT e.*,
       distance_km(me.latitude, me.longitude, e.latitude, e.longitude) AS distance_km
FROM events e
CROSS JOIN (SELECT latitude, longitude FROM places WHERE place = :'my_place') AS me
WHERE (:radius_km = 0 OR distance_km(me.latitude, me.longitude, e.latitude, e.longitude) <= :radius_km)
  AND (:'my_category' = 'All' OR e.category = :'my_category');

SELECT :'my_place' AS chosen_place, :radius_km AS radius_km, :'my_category' AS category,
       COUNT(*) AS events_in_range
FROM near_me;

SELECT category,
       COUNT(*) AS activities,
       SUM(CASE WHEN is_free THEN 1 ELSE 0 END)  AS free_activities,
       ROUND(100.0 * AVG(CASE WHEN is_free THEN 1 ELSE 0 END), 0) AS pct_free,
       ROUND(AVG(price_rand) FILTER (WHERE NOT price_imputed AND NOT is_free), 0) AS avg_paid_price,
       MIN(distance_km) AS nearest_km
FROM near_me
GROUP BY category
ORDER BY activities DESC;

SELECT place,
       ROUND(AVG(distance_km), 1)  AS distance_km,
       COUNT(*)  AS upcoming_activities,
       COUNT(DISTINCT category) AS categories,
       ROUND(AVG(price_rand), 0) AS avg_price,
       RANK() OVER (ORDER BY COUNT(*) DESC) AS busiest_rank
FROM near_me
WHERE COALESCE(end_date, start_date) >= CURRENT_DATE OR event_type = 'Recurring'
GROUP BY place
ORDER BY upcoming_activities DESC, distance_km;

SELECT start_date, end_date, start_time, event_name, category, place, distance_km,
       CASE WHEN is_free THEN 'Free'
            WHEN price_imputed THEN 'approx. R' || price_rand::INT
            ELSE 'R' || price_rand::INT END AS price
FROM near_me
WHERE COALESCE(end_date, start_date) >= CURRENT_DATE OR event_type = 'Recurring'
ORDER BY start_date, distance_km
LIMIT 30;

SELECT start_date, start_time, event_name, category, place, distance_km,
       CASE WHEN is_free THEN 'Free'
            WHEN price_imputed THEN 'approx. R' || price_rand::INT
            ELSE 'R' || price_rand::INT END  AS price,
       COALESCE(LEFT(summary, 200), '(no description)') AS what_is_it
FROM near_me
WHERE COALESCE(end_date, start_date) >= CURRENT_DATE OR event_type = 'Recurring'
ORDER BY start_date, distance_km
LIMIT 212;