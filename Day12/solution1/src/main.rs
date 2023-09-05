use std::fs;
use std::collections::HashMap;
use std::collections::VecDeque;

const FILE_PATH: &str = "Input.txt";
//const WIDTH : usize = 8;
//const HEIGHT: usize = 5;
const WIDTH : usize = 95;
const HEIGHT: usize = 41;
const START: char = 'S';
const END  : char = 'E';

// 421 too high

fn main() {

    let mut map = read_file(FILE_PATH.to_string());

    let path = pathfind(map);
    println!("{path:?}, {}", path.len());
}

fn pathfind(mut map: Vec<Vec<char>>) -> Vec<(usize, usize)> {
    let start = find_start(&map);
    let end = find_end(&map);

    println!("start: {:?}\nend: {:?}", start, end);

    let mut table = HashMap::new();
    let mut queue = Vec::new();

    map = 
        map
            .into_iter()
            .map(|x| 
                x
                    .into_iter()
                    .map(|y| 
                        {
                        if y == START { 
                            'a' 
                        } 
                        else if y == END {
                            'z' 
                        } else {
                            y 
                        }
                        }
                    ).collect()
            ).collect();

    let alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];

    assert!(map.iter().all(|xs| xs.iter().all(|x| alphabet.contains(x))));

    println!("{map:?}");

    for x in 0..WIDTH {
        for y in 0..HEIGHT {
            if (x, y) != start {
                table.insert((x, y), Point::new(x, y, &map));
                queue.push((x, y));
            }
        }
    }


    table.insert(start, Point::start(start.0, start.1, &map));
    queue.push(start);

    while table.iter().any(|(_, x)| !x.done) {
    //while !table[&end].done {
    //while queue.len() > 0 {
        // Sort table
        queue.sort_by(|a, b| table[a].cost.cmp(&table[b].cost));

        println!("{} elements left", queue.len());

        // See if you're best for your neighbors
        let current_point = queue.first().unwrap().clone();
        let current = table.get(&current_point).unwrap().clone();

        println!("\nCurrent {current:?}");
        for neighbor_point in current.neighbors.iter() {
            let neighbor = table.get_mut(neighbor_point).unwrap();

            println!("\tNeighbor {neighbor:?}");

            //if current.cost < neighbor.cost - 1 {
            if current.cost + 1 < neighbor.cost {
                neighbor.cost = current.cost + 1;
                neighbor.route = Some(current_point);
            }
        }

        table.get_mut(&current_point).unwrap().done = true;

        // remove dones
        //queue = queue.into_iter().filter(|x| !table[x].done).collect();
        queue.retain(|x| *x != current_point);
    }

    // Build Path
    
    let mut path: Vec<(usize, usize)> = Vec::new();
    let mut current = end;

    while current != start {
        path.push(current);    
        println!("{current:?} -> route: {:?}", table[&current].route);
        current = table[&current].route.unwrap();
        /*
        let current = match table[&current].route {
            Some(x) => x,
            None    => panic!("(.Y.) {path:?}"),
        };
        */

    }

    path.reverse();

    return path;
}

fn find_start(map: &Vec<Vec<char>>) -> (usize, usize) {

    const NEEDLE: char = START;

    for x in 0..WIDTH {
        for y in 0..HEIGHT {
            if map[y][x] == NEEDLE {
                return (x, y);
            }
        }
    }

    panic!("Couldn't find \'{NEEDLE}\'");
}

fn find_end(map: &Vec<Vec<char>>) -> (usize, usize) {

    const NEEDLE: char = END;

    for x in 0..WIDTH {
        for y in 0..HEIGHT {
            if map[y][x] == NEEDLE {
                return (x, y);
            }
        }
    }

    panic!("Couldn't find \'{NEEDLE}\'");
}

fn read_file(file_name: String) -> Vec<Vec<char>> {
    let mut contents = fs::read_to_string(file_name)
        .expect("Should have been able to read the file");

    contents
        .lines()
        .collect::<Vec<&str>>()
        .into_iter()
        .map(|x| 
            x.chars().collect::<Vec<char>>()
    ).collect()

}

fn neighbors(x: usize, y: usize) -> Vec<(usize, usize)> {
    let up    = (x as isize, y as isize - 1);
    let down  = (x as isize, y as isize + 1);
    let left  = (x as isize - 1, y as isize);
    let right = (x as isize + 1, y as isize);

    let neighbors = vec![up, down, left, right];

    neighbors
        .into_iter()
        .filter(|i| in_range(*i))
        .map(|(nx, ny)| (nx as usize, ny as usize)).collect()
}

fn in_range(coord: (isize, isize)) -> bool {
    let (x, y) = coord;
    x >= 0 && x < (WIDTH as isize) && y >= 0 && y < (HEIGHT as isize)
}

#[derive(Debug, Clone)]
struct Point {
    x: usize,
    y: usize,

    route: Option<(usize, usize)>,

    cost: usize,

    done: bool,

    neighbors: Vec<(usize, usize)>,
}

impl Point {
    fn new(x: usize, y: usize, map: &Vec<Vec<char>>) -> Point {

        let neighbors: Vec<(usize, usize)> = 
            neighbors(x, y)
            .into_iter()
            .filter(|(nx, ny)| 
                map[y][x] as u8 + 1 >= map[*ny][*nx] as u8
            )
            .collect();

        assert!(neighbors.iter().all(|i| *i != (x, y)));

        Point {
            x,
            y,
            route: None,
            cost: usize::MAX,
            done: false,
            neighbors,
        }
    }

    fn start(x: usize, y: usize, map: &Vec<Vec<char>>) -> Point {
        let mut point = Point::new(x, y, map);

        point.cost = 0;

        point
    }
}
