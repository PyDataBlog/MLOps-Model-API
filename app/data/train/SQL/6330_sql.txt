create table RecipesToTypes
(
	Id           int not null,
	RecipeId     int not null,
    TypeId       int not null
    primary key clustered ([Id] ASC),
    constraint FK_RecipesToTypes_ToRecipes   foreign key ([RecipeId]) references Recipes ([Id]), 
    constraint FK_RecipesToTypes_ToDishTypes foreign key ([TypeId])   references DishTypes([Id])
)
