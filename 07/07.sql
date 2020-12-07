DROP TABLE IF EXISTS raw_input;
DROP TABLE IF EXISTS bag_pairs;

CREATE TABLE raw_input (
  raw_input_line TEXT NOT NULL
);

\COPY raw_input FROM 'input.txt';

CREATE TABLE bag_pairs AS
WITH
  split_containing_bag AS (
    SELECT 
      REGEXP_SPLIT_TO_ARRAY(raw_input_line, ' bags contain ') AS split_line
    FROM
      raw_input
  ),
  split_contained_bags AS (
    SELECT
      split_line[1] AS containing_bag_colour,
      REGEXP_SPLIT_TO_TABLE(TRIM(split_line[2], '.'), ', ') AS contained_bags
    FROM
      split_containing_bag
  ),
  containing_bag_pairs AS (
    SELECT
      containing_bag_colour,
      REGEXP_MATCH(contained_bags, '^(\d+) (.+) bags?$') AS contained_bags_match
    FROM
      split_contained_bags
  )
SELECT
  containing_bag_colour,
  contained_bags_match[2] AS contained_bag_colour,
  contained_bags_match[1]::INT AS contained_bag_count 
FROM
  containing_bag_pairs;

WITH RECURSIVE possible_outer_bags AS (
  SELECT
    containing_bag_colour AS outer_bag,
    contained_bag_colour AS inner_bag
  FROM
    bag_pairs
  WHERE
    contained_bag_colour = 'shiny gold'
  UNION
  SELECT
    containing_bag_colour,
    contained_bag_colour
  FROM 
    bag_pairs
    JOIN possible_outer_bags ON (
      bag_pairs.contained_bag_colour = possible_outer_bags.outer_bag
    )
)
SELECT COUNT(DISTINCT outer_bag)
FROM possible_outer_bags;

WITH RECURSIVE required_inner_bags AS (
  SELECT
    containing_bag_colour AS outer_bag,
    contained_bag_colour AS inner_bag,
    contained_bag_count AS inner_bag_count
  FROM
    bag_pairs
  WHERE
    containing_bag_colour = 'shiny gold'
  UNION
  SELECT
    containing_bag_colour,
    contained_bag_colour,
    bag_pairs.contained_bag_count * required_inner_bags.inner_bag_count
  FROM 
    bag_pairs
    JOIN required_inner_bags ON (
      bag_pairs.containing_bag_colour = required_inner_bags.inner_bag
    )
)
SELECT SUM(inner_bag_count)
FROM required_inner_bags;
