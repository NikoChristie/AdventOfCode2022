use std::collections::HashMap;

const CONTENTS: [&str; 23] = [
    "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
];

#[derive(Debug)]
enum File {
    Folder((String, Vec<File>)),
    Document((String, usize)),
}

fn parse_commands() {

    let mut contents: Vec<String> = 
        CONTENTS.into_iter()
                .filter_map(|i| 
                    if i == "$ cd .." || i == "$ ls" {
                        None
                    }
                    else {
                        Some(i.to_string())
                    }
                )
                .collect();

    let mut filesystem = 
        HashMap::from([
            ("/".to_owned(), 
             File::Folder(("/".to_owned(), Vec::new()))
            )
        ]);
    let mut current: String = "".to_owned();

    for content in contents.iter() {
        if content.starts_with("$ cd ") {
            let filename: String = content.chars().skip(5).collect(); 
            println!("cd {}", filename);
            current = filename;
        }
        else if content.starts_with("dir ") {
            let filename: String = content.chars().skip(4).collect();
            println!("dir {}", filename);

            filesystem.insert(filename.clone(), File::Folder((filename, vec![])));
        }
        else {
            let mut split: Vec<_> = content.split_whitespace().collect();

            let size: usize  = split[0].parse().unwrap();
            let name: String = split[1].to_string();

            println!("document \"{}\" of size {}", name, size);

            println!("Parent {}", current);
            let parent = filesystem.get_mut(&current).unwrap();

            match parent {
               File::Folder((_, children)) => children.push(File::Document((name, size))), 
               _ => panic!("(.Y.)"),
            }
        }
    }

    println!("{:#?}", filesystem);

    println!("a {}", sum_of_size(&filesystem, &"a".to_owned()));
}

fn sum_of_size(fs: &HashMap<String, File>, file: &String) -> usize {
    let mut acc = 0;

    let file = fs.get(file).unwrap();

    match file {
        File::Folder((name, children)) => {
            children.iter().for_each(|i| 
                match i {
                    File::Folder((n, c)) => acc += sum_of_size(fs, n),
                    File::Document((_, size)) => acc += size,
                }                
            ); 
        }
        File::Document((_, size)) => acc += size,
    }
    acc
}

fn main() {
    parse_commands();
}
