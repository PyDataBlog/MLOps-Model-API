class Solution {

    public int solution(int[] A) {
        
        int[] temArray = new int[A.length + 2];
    		 
    	 	for (int i = 0; i < A.length; i++) {
    	 		temArray[A[i]] = 1;
    		}
            
    	 	for (int i = 1; i < temArray.length + 1; i++) {
    	 		if(temArray[i] == 0){
    	 			return i;
    	 		}
    		}
  	 	
        return A[A.length - 1] + 1;
        
    }
}
