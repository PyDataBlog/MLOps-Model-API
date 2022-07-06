fn main() {
    let mut my_struct = MyStruct {
        my_field1: 0,
        my_field2: 0,
        my_field3: 0,
        my_field4: 0,
        my_field5: 0,
    };

    my_struct.my_field1 = 7;

    assert_eq!(7, my_struct.my_field1);
    assert_eq!(0, my_struct.my_field2);
    assert_eq!(0, my_struct.my_field3);
    assert_eq!(0, my_struct.my_field4);
    assert_eq!(0, my_struct.my_field5);

    let my_struct = my_struct; // set my_struct to be immutable
    let my_struct2 = MyStruct { my_field3: 5, ..my_struct }; // copy unspecified fields

    assert_eq!(7, my_struct2.my_field1);
    assert_eq!(0, my_struct2.my_field2);
    assert_eq!(5, my_struct2.my_field3);
    assert_eq!(0, my_struct2.my_field4);
    assert_eq!(0, my_struct2.my_field5);

    let my_tuple_struct = MyTupleStruct(7, 42, 3.14);
    assert_eq!(7, my_tuple_struct.0);
    assert_eq!(42, my_tuple_struct.1);
    assert_eq!(3.14, my_tuple_struct.2);
}

struct MyStruct {
    my_field1: i32,
    my_field2: i32,
    my_field3: i32,
    my_field4: i32,
    my_field5: i32,
}

struct MyTupleStruct(i32, i32, f32);
