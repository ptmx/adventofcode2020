use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;
use std::io::Result;
use std::iter::FromIterator;

fn main() -> Result<()> {
  let file = File::open("input.txt")?;
  let reader = BufReader::new(file);

  let mut allergen_ingredients: HashMap<String, HashSet<String>> = HashMap::new();
  let mut ingredient_count = HashMap::new();
  for line in reader.lines() {
    let parts: Vec<String> = line.unwrap().split(" (contains ").map(|s| s.to_string()).collect();
    let ingredient_list = parts[0].clone();
    let mut allergen_list = parts[1].clone();
    allergen_list.pop();

    let ingredients: Vec<String> = ingredient_list.split(" ").map(|s| s.to_string()).collect();
    let allergens: Vec<String> = allergen_list.split(", ").map(|s| s.to_string()).collect();

    let ingredients_set: HashSet<String> = HashSet::from_iter(ingredients.iter().cloned());
    for allergen in &allergens {
      let new_candidates = match allergen_ingredients.get(allergen) {
        Some(c) => c.intersection(&ingredients_set).map(|s| s.to_string()).collect(),
        None => ingredients_set.clone()
      };
      allergen_ingredients.insert(allergen.to_string(), new_candidates);
    }

    for ingredient in &ingredients {
      let count = match ingredient_count.get(ingredient) {
        Some(c) => *c,
        None => 0
      };
      ingredient_count.insert(ingredient.to_string(), count + 1);
    }
  }

  let all_possible_allergen_ingredients = allergen_ingredients
    .values()
    .fold(HashSet::new(), |acc, ingredients| {
      acc.union(&ingredients).map(|s| s.to_string()).collect()
    });
  
  let inert_count = ingredient_count
    .iter()
    .filter(|&(ingredient, _)| !all_possible_allergen_ingredients.contains(ingredient))
    .fold(0, |acc, (_, count)| acc + count);

  // Part 1
  println!("{}", inert_count);

  while allergen_ingredients.values().any(|ingredients| ingredients.len() > 1) {
    let next_known_allergen = allergen_ingredients
      .values()
      .find(|ingredients| ingredients.len() == 1)
      .unwrap()
      .to_owned();

    allergen_ingredients = allergen_ingredients
      .into_iter()
      .map(|(allergen, ingredients)| {
        if ingredients.len() > 1 {
          (allergen, ingredients.difference(&next_known_allergen).map(|s| s.to_string()).collect())
        } else {
          (allergen, ingredients)
        }
      })
      .collect();
  }

  let mut allergens: Vec<String> = allergen_ingredients.keys().map(|s| s.to_string()).collect();
  allergens.sort();

  let ingredients: Vec<String> = allergens.iter().map(|allergen| {
    let ingredient = allergen_ingredients.get(allergen);
    match ingredient {
      Some(i) => i.iter().next().unwrap(),
      None => panic!("Unknown allergen ingredient")
    }
  }).map(|s| s.to_string()).collect();

  // Part 2
  println!("{}", ingredients.join(","));

  Ok(())
}
