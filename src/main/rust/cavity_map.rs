// Cavity Map
// https://www.hackerrank.com/challenges/cavity-map

#![feature(slicing_syntax)]
use std::cmp::Ordering;
fn main() {
    let mut stdin = std::io::stdio::stdin();
    let line = stdin.read_line().ok().unwrap();
    let n = line.trim().parse().unwrap();
    let mut arr = [[0; 100]; 100];

    for i in range(0, n) {
        let s = stdin.read_line().ok().unwrap();
        for j in range(0, n) {
            arr[i][j] = s.as_slice()[j..j+1].parse().unwrap();
        }
    }

    let mut min_d = [[0; 100]; 100];
    for i in range(0, n-1) {
        for j in range(0, n-1) {
            match arr[i][j].cmp(&arr[i][j+1]) {
                Ordering::Greater => min_d[i][j] += 1,
                Ordering::Less    => min_d[i][j+1] += 1,
                Ordering::Equal   => {}
            }
            match arr[i][j].cmp(&arr[i+1][j]) {
                Ordering::Greater => min_d[i][j] += 1,
                Ordering::Less    => min_d[i+1][j] += 1,
                Ordering::Equal   => {}
            }
        }
    }

    let mut s: String = String::new();
    for row in range(0, n) {
        for col in range(0, n) {
            print!("{}", (if min_d[row][col] == 4 { "X" } else { s = arr[row][col].to_string(); s.as_slice() }))
        }
        println!("")
    }
}

