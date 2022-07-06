local object = require("object")
local null = require("Null"):new().null
local Array=object:extend(function(class)
class.elements={}--таблица для массива, не рекомендуется хранить в ней значения со строковыми ключами
function object:init(elements)--конструктор,если хоттите создать пусой массив передайте аргумент {}
self.elements=elements
end
function class:push(objectA)--вставка в конец массива, возвращает новую длину
if(objectA!=nil)then
self.elements[#self.elements+1]=objectA
end
return #self.elements

end

function class:lenght()--длина массива
return #self.elements

end

function class:concat(array)--сложение массива с таблицей
do
local arr=self.elements
for(i in array)do
self:push(i)
end
return arr
end
end

function class:pop()--удаление последнего элемента
do 
local el=self.elements[#self.elements]
self.elements[#self.elements]=nil
end
return el
end

function class:shift()--удаление первого элемента
do
local arr={}
for(i=2,#self.elements)do
arr[i-1]=self.elements[i]
end
local el=self.elements[1]
self.elements=arr
end
return el
end

function class:slice(posA,posB)--вырезает массив между позициями
do
local arr={}
for(i=posA,posB)do
arr[#arr]=self.elements[i]
end
return Array:new(arr)
end

end

function class:toString()--строковое представление, используйте вместо tostring
do
local arr=""
for(i=1,#self.elements)do
arr=arr..tostring(self.elements[i])
end
return arr
end
end

function class:unshift(objectA)--вставка в начало массива, возвращает новую длину
do
local arr={objectA}
for(i=1,#self.elements)do
arr[#arr+1]=self.elements[i]
end
self.elements=arr
end
return #self.elements
end

function class:insert(pos,x)--вставляет элемент x в позицию pos, если pos<0, то позиция считается от конца, если pos>lenght, то все промежутки заполняются значение null
do
local arr={}
if(pos>0)then
if(pos>#self.elements)then
for(i=#self.elements+1,pos-1)do
self.elements[i]=null
end
self.elements[pos]=x
return
elseif(pos<#self.elements)then
self.elements=self:insertHelp(arr,pos,x)
return
end
elseif(pos<0)then
self.elements=self:insertHelp(arr,pos,x)
return
end
end
end

function class:remove(x,requiredShift=false)--удаляет первое вхождение элемента x, если requiredShift==false, на место первой позиции x будет значение null
do
local arr={}
local pos=self:indexOf(x)
if(pos==-1)then return false end
if(requiredShift)then
arr=self:slice(1,pos-1).elements
local arrr=Array:new(arr)
arr=self:slice(pos+1,#self.elements).elements
arrr:concat(arr)
self.elements=arrr.elements
return true
else
self.elements[pos]=null
return true
end
end
end

function class:indexOf(x,fromIndex=0)--возвращает индекс первого вхождения элемента x, начитая от fromIndex
if(fromIndex==0)then
for(i=1,#self.elements)do
if(self.elements[i]==x)then return i end
end
else
local arrr=Array:new(self:slice(fromIndex,#self.elements).elements)
return arrr:indexOf(x)
end
return -1
end

function class:filter(f)--фильтр, возвращает новый массив согласно f. f - функция, принимающая значение массива и возвращающая значение boolean(true, если элемент остается)
do
local arr={}
for(i in self.elements)do
if(f(i))then
arr[#arr+1]=i
end
end
return Array:new(arr)

end

end


function class:insertHelp(arr,pos,x)--вспомогательная функция
arr=self:slice(1,pos-1).elements
arr[pos]=x
for(i=pos+1,#self.elements-pos)do
arr[#arr]=self.elements[i]
end
return arr

end

end)
return Array