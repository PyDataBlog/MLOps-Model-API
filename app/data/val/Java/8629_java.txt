
class CountUniqueIntegers {
	private Set<Integer> set;

	public CountIntegers( Set<Integer> set ) {
		this.set = set;
	}

	public int getCount( int[] array ) {
		int length = array.length;
		
		for( int i = 0; i < length; i ++ ) 
			this.set.push( array[i] );

		return this.set.size();
	}
}
