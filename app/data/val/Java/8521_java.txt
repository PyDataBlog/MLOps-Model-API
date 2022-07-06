package trubbl.issues.domain;

import com.fasterxml.jackson.annotation.JsonProperty;

import trubbl.sharedkernel.domain.Key;
import trubbl.sharedkernel.domain.Keyable;

public class Issue implements Keyable {

	@JsonProperty
	private Key key;
	@JsonProperty
	private String title;
	@JsonProperty
	private String description;

	public Issue(String title, String description) {
		this();
		this.title = title;
		this.description = description;
	}

	public Key key() {
		return this.key;
	}

	// REST
	public Issue() {
		this.key = new Key();
	}
}
