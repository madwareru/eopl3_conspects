extern crate core;

use std::collections::HashMap;
use std::rc::Rc;

pub mod chapter_1;
pub mod chapter_2;
pub mod chapter_3;

#[derive(Clone)]
enum VoxelModel<TData : Copy, const CHUNK_SIZE: usize> {
    Leaf(TData),
    Node(Rc<[[VoxelModel<TData, CHUNK_SIZE>; CHUNK_SIZE]; CHUNK_SIZE]>)
}

#[macro_export]
macro_rules! do_tests(
    ($test_name: ident; $($label: literal <$($t: ty),+>),+) => {
        $(
            println!("testing {}:", $label);
            $test_name::<$($t),+>();
            println!();
        )+
    }
);

macro_rules! make_menu(
    ($($label:literal: $foo_path:expr),+) => {
        {
            let mut menu: HashMap<&'static str, fn()> = HashMap::new();
            menu.extend([
                $(($label, $foo_path as fn())),+
            ]);
            menu
        }
    }
);

fn main() {
    let menu = make_menu![
        "2.1": chapter_2::exercise_1::ex_2_1,
        "2.5": chapter_2::exercise_5::ex_2_5,
        "2.15": chapter_2::exercise_15::ex_2_15,
        "2.29": chapter_2::exercise_29::ex_2_29,
        "3.let": chapter_3::let_language::ex_3_let,
        "3.let proc": chapter_3::let_proc_language::ex_3_let_proc,
        "3.imperative": chapter_3::imperative_language::ex_3_imperative_lang
    ];

    let mut buffer = String::new();
    let stdin = std::io::stdin();

    loop {
        println!("+------------------------------------+");
        println!("| EOPL 3d edition exercise solutions |");
        println!("+------------------------------------+");
        println!("type --list for list of all solutions");
        println!("type --quit                  for quit");
        println!("type a solution name        to run it");

        buffer.clear();
        stdin.read_line(&mut buffer).unwrap();
        buffer.retain(|c| c != '\r' && c != '\n');

        let command = &buffer as &str;

        match command {
            "--quit" => {
                println!("good bye!");
                break;
            },
            "--list" => {
                println!("exercise solutions:");
                for &key in menu.keys().into_iter() {
                    println!("  {key}");
                }
                println!();
            }
            _ => {
                match menu.get(command) {
                    None => {
                        println!("An example with a name \"{command}\" not found")
                    }
                    Some(foo) => {
                        foo();
                        println!("exercise {command} is complete\n");
                    }
                }
            }
        }
    }
}
