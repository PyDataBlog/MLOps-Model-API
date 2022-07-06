IF (EXISTS (SELECT * 
			FROM INFORMATION_SCHEMA.TABLES 
                 WHERE TABLE_SCHEMA = 'customer' 
                 AND  TABLE_NAME = 'cart'))
BEGIN
    print 'Table: customer.cart already exists.';
END
else
begin
	CREATE TABLE [customer].cart(
		cartId BIGINT not null,
		customerId BIGINT not null,
		created datetime not null

		CONSTRAINT PK_CustomerCart_CartId PRIMARY KEY CLUSTERED (cartId)
)  	

end