**IsLoginAvailable**
----
  Check whether a login is avaible or not.

* **URL**
  https://fr.oopad.com/WebServices/SignUpWebService/IsLoginAvailable

* **Method:**
  `POST` 
  
*  **URL Params**
	none

* **Data Params**
	- login
		`type`: string
		`description`: login to check

	**example:**
```json
{
  "login" : "test"
}
```

* **Success Response:**
    **Code:** 200 
    **Content:** 
    
    - *anonym*
	    `type`: bool
	    `description`: `true` if available

	**example:**
```json
{
    "d": true
}
```
 
* **Error Response:**
	//TODO
	
* **Sample Call:**
	//TODO
	
* **Notes:**
	//TODO

> Written with [StackEdit](https://stackedit.io/).