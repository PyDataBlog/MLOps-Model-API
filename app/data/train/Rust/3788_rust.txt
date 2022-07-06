//! Module contains pure implementations of bubble sort algorithm.
//!
//! * `sort` - is non distructive function (does not affects on input sequence)
//! * `mut_sort` - function with side effects (modify input sequence)
//!
//! It's better to use first one in functional style, second - in OOP-style

/// Pure implementation of Bubble sort algorithm
///
/// # Examples
///
/// ```
/// use algorithms::sorts::bubble_sort::sort;
/// use algorithms::sorts::utils::is_sorted;
///
/// assert!(is_sorted(&sort(&[0, 5, 3, 2, 2])));
/// assert!(is_sorted(&sort(&[-1, -5, -10, 105])));
/// ```
pub fn sort<T: Ord + Clone>(list: &[T]) -> Vec<T> {
    let mut sorted_list = list.clone().to_vec();
    let length = sorted_list.len();
    for i in 0..length {
        let mut swapped = false;
        for j in 0..length - i - 1 {
            if sorted_list[j] > sorted_list[j + 1] {
                sorted_list.swap(j, j + 1);
                swapped = true;
            }
        }
        if swapped == false {
            break;
        }
    }
    return sorted_list;
}

/// Pure implementation of bubble sort algorithm. Side-effects version
/// Modifies input sequense
///
/// # Examples
///
/// ```
/// use algorithms::sorts::bubble_sort::mut_sort;
/// use algorithms::sorts::utils::is_sorted;
///
/// let mut test_sequence = [0, 5, 3, 2, 2];
/// mut_sort(&mut test_sequence);
/// assert!(is_sorted(&test_sequence));
///
/// let mut test_sequence = [0, 5, 3, 2, 2];
/// mut_sort(&mut test_sequence);
/// assert!(is_sorted(&test_sequence));
/// ```
pub fn mut_sort<T: Ord>(list: &mut [T]) {
    for n in (0..(list.len() as isize + 1)).rev() {
        for m in 1..(n as usize) {
            if list[m] < list[m - 1] {
                list.swap(m, m - 1);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::utils::is_sorted;

    #[test]
    fn sort_numbers() {
        let empty_vec: Vec<i32> = Vec::new();
        assert!(is_sorted(&sort(&[0, 5, 3, 2, 2])));
        assert!(is_sorted(&sort(&empty_vec)));
        assert!(is_sorted(&sort(&[-1, -5, -10, 105])));
    }

    #[test]
    fn sort_chars() {
        assert!(is_sorted(&sort(&['a', 'r', 'b'])))
    }

    #[test]
    fn mut_sort_numbers() {
        let mut test_vector = vec![0, 5, 2, 3, 2];
        mut_sort(&mut test_vector);
        assert!(is_sorted(&test_vector));
        let mut test_vector: Vec<i32> = Vec::new();
        mut_sort(&mut test_vector);
        assert!(is_sorted(&test_vector));
    }

    #[test]
    fn mut_sort_chars() {
        let mut test_vector = vec!['a', 'r', 'b'];
        mut_sort(&mut test_vector);
        assert!(is_sorted(&test_vector));
    }
}
