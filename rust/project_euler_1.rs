// Project Euler #1: Multiples of 3 and 5
// https://www.hackerrank.com/contests/projecteuler/challenges/euler001

use std::iter::range_step;
use std::collections::BTreeSet;
use std::collections::BTreeMap;
fn main() {
    let mut stdin = std::io::stdio::stdin();
    let count = stdin.read_line().ok().unwrap().trim().parse().unwrap();
    let mut numbers: Vec<i64> = Vec::new();
    let mut results: Vec<i64> = Vec::new();
    let mut numMap: BTreeMap<i64, Vec<uint>> = BTreeMap::new();
    for i in range(0, count) {
        let n: i64 = stdin.read_line().ok().unwrap().trim().parse().unwrap();
        numbers.push(n);
        results.push(0);
        if (!numMap.contains_key(&n)) {
            numMap.insert(n, Vec::new());
        }
        numMap[n].push(i);
    }

    let mut ord_numbers = numbers.clone();
    ord_numbers.sort();
    let num_set: BTreeSet<i64> = ord_numbers.iter().map(|&x| x).collect();

    let mut sum = 0i64;
    let mut prev = 0i64;
    for &n in num_set.iter() {
        for i in range_step(((prev + 2) / 3) * 3, n, 3) {
            sum += i;
        }
        for i in range_step(((prev + 4) / 5) * 5, n, 5) {
            if i % 3 != 0 {
                sum += i;
            }
        }
        prev = n;

        let vec: &Vec<uint> = numMap.get(&n).unwrap();
        for &j in vec.iter() {
            results[j] = sum;
        }
    }

    for r in results.iter() {
        println!("{}", r);
    }
}
