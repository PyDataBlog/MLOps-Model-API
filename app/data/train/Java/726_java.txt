package gof.structure.proxy;

public class ProxySubject extends Subject {

	private RealSubject realSubject;
	
	public ProxySubject(){
		
	}
	
	/* (non-Javadoc)
	 * @see gof.structure.proxy.Subject#request()
	 * 
	 * Subject subject = new ProxySubject();
	 * subject.request();
	 */
	@Override
	public void request() {

		preRequest();
		if(realSubject == null){
			realSubject = new RealSubject();
		}
		realSubject.request();
		postRequest();
	}
	
	private void preRequest(){
		
	}
	
	private void postRequest(){
		
	}

}
