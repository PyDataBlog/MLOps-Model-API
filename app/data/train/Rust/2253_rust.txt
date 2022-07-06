use std::cmp::Ordering;

pub enum SortingAlgorithmn {
    Bubble,
    Quick
}

pub trait Sort {

    fn adv_sort_mut(&mut self, algo: SortingAlgorithmn);
}

pub trait SortBy<T: PartialOrd, F: FnMut(&T, &T)
    -> Ordering> {

    fn adv_sort_by_mut(&mut self, compare: &mut F, algo: SortingAlgorithmn);
}

pub fn bubble_sort_mut<T: Ord + Clone>(ary: &mut [T]) {

    for i in 0..ary.len() - 1 {
        for j in 0..ary.len() - i - 1 {
            if ary[j] > ary[j + 1] {
                ary.swap(j, j + 1);
            }
        }
    }
}

pub fn bubble_sort_by_mut<T, F: FnMut(&T, &T)
    -> Ordering>(compare: &mut F, ary: &mut [T]) {

    for i in 0..ary.len() - 1 {
        for j in 0..ary.len() - i - 1 {
            if compare(&ary[j], &ary[j + 1]) == Ordering::Greater {
                ary.swap(j, j + 1);
            }
        }
    }
}

pub fn quick_sort_mut<T: Ord + Clone>(ary: &mut [T]) {

    if 1 < ary.len() {
        let (mut p, mut x) = (0, ary.len()-1);
        for _ in 0..ary.len() - 1 {
            if ary[p] < ary[p + 1] {
                ary.swap(p + 1, x);
                x -= 1;
            }else{
                ary.swap(p, p + 1);
                p += 1;
            }
        }

        quick_sort_mut(&mut ary[..p]);
        quick_sort_mut(&mut ary[p + 1..]);
    }
}


pub fn quick_sort_by_mut<T, F: FnMut(&T, &T)
    -> Ordering>(compare: &mut F, ary: &mut [T]) {

    if 1 < ary.len() {
        let (mut p, mut x) = (0, ary.len()-1);
        for _ in 0..ary.len() - 1 {
            if compare(&ary[p], &ary[p + 1]) == Ordering::Less {
                ary.swap(p + 1, x);
                x -= 1;
            }else{
                ary.swap(p, p + 1);
                p += 1;
            }
        }

        quick_sort_by_mut(compare, &mut ary[..p]);
        quick_sort_by_mut(compare, &mut ary[p + 1..]);
    }
}
