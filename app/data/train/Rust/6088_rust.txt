use std::collections::HashMap;

fn main() {
    let mut scores = HashMap::new();
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    let teams = vec![String::from("Blue"), String::from("Yellow")];
    let initial_scores = vec![10, 50];

    let different_scores: HashMap<_, _> = teams.iter().zip(initial_scores.iter()).collect();

    // Hash map becomes the owner of values used during inserting

    let team_name = String::from("Blue");
    let score = different_scores.get(&team_name);

    for (key, value) in &different_scores {
        println!("{}: {}", key, value);
    }

    // Insert overwrites things by default

    scores.entry(String::from("Blue")).or_insert(100);
    scores.entry(String::from("Red")).or_insert(200);

    println!("{:?}", scores);

    let text = "hello wonderful world world";

    let mut map = HashMap::new();

    for word in text.split_whitespace() {
        let count = map.entry(word).or_insert(0)
        *count += 1;
    }

    println!("{:?}", map);

    // It's possible to use different hasher than default one
}
