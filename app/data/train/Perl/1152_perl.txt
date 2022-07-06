% *************************************************************************** %
%
%  Copyright (c) 2006-2019 Clifford Thompson
%  
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%
% *************************************************************************** %

% ******************************** %
%                                  %
%      Class: MCPS 791             %
%      Prof : Dr.Charles Brown     %
%      File : adjectives.pl        % 
%                                  %
% ******************************** %


%adj(adj(),[cat:adj,sem:,sound:,pos:]) --> [].

adj(adj(alone),         [cat:adj,sem:alone:1,         sound:soft,pos:_Pos]) --> [alone].
adj(adj(angry),         [cat:adj,sem:angry:1,         sound:soft,pos:_Pos]) --> [angry].
adj(adj(backwards),     [cat:adj,sem:backwards:1,     sound:hard,pos:_Pos]) --> [backwards].
adj(adj(beautiful),     [cat:adj,sem:beautiful:1,     sound:hard,pos:_Pos]) --> [beautiful].
adj(adj(best),          [cat:adj,sem:best:1,          sound:hard,pos:_Pos]) --> [best].
adj(adj(bright),        [cat:adj,sem:bright:1,        sound:hard,pos:_Pos]) --> [bright].
adj(adj(content),       [cat:adj,sem:content:1,       sound:hard,pos:_Pos]) --> [content].
adj(adj(distinguished), [cat:adj,sem:distinguished:1, sound:hard,pos:_Pos]) --> [distinguished].
adj(adj(dry),           [cat:adj,sem:dry:1,           sound:hard,pos:_Pos]) --> [dry].
adj(adj(good),          [cat:adj,sem:good:1,          sound:hard,pos:_Pos]) --> [good].
adj(adj(finished),      [cat:adj,sem:finished:1,      sound:hard,pos:_Pos]) --> [finished].
adj(adj(forwards),      [cat:adj,sem:forwards:1,      sound:hard,pos:_Pos]) --> [forwards].
adj(adj(full),          [cat:adj,sem:full:1,          sound:hard,pos:_Pos]) --> [full].
adj(adj(incorrect),     [cat:adj,sem:incorrect:1,     sound:hard,pos:_Pos]) --> [incorrect].
adj(adj(ill),           [cat:adj,sem:ill:1,           sound:hard,pos:_Pos]) --> [ill].
adj(adj(intensely),     [cat:adj,sem:intensely:1,     sound:hard,pos:pred]) --> [intensely].
adj(adj(handsomely),    [cat:adj,sem:handsomely:1,    sound:hard,pos:pred]) --> [handsomely].
adj(adj(happily),       [cat:adj,sem:happily:1,       sound:hard,pos:pred]) --> [happily].
adj(adj(hard),          [cat:adj,sem:hard:1,          sound:hard,pos:_Pos]) --> [hard].
adj(adj(honest),        [cat:adj,sem:honest:1,        sound:soft,pos:_Pos]) --> [honest].
adj(adj(made),          [cat:adj,sem:made:1,          sound:hard,pos:pred]) --> [made].
adj(adj(new),           [cat:adj,sem:new:1,           sound:hard,pos:_Pos]) --> [new].
adj(adj(nice),          [cat:adj,sem:nice:1,          sound:hard,pos:_Pos]) --> [nice].
adj(adj(old),           [cat:adj,sem:old:1,           sound:hard,pos:_Pos]) --> [old].
adj(adj(pointed),       [cat:adj,sem:pointed,         sound:hard,pos:_Pos]) --> [pointed].
adj(adj(poor),          [cat:adj,sem:poor,            sound:hard,pos:_Pos]) --> [poor].
adj(adj(pretty),        [cat:adj,sem:pretty,          sound:hard,pos:_Pos]) --> [pretty].
adj(adj(same),          [cat:adj,sem:same,            sound:hard,pos:_Pos]) --> [same].
adj(adj(sick),          [cat:adj,sem:sick,            sound:hard,pos:_Pos]) --> [sick].
adj(adj(skillfull),     [cat:adj,sem:skillfull,       sound:hard,pos:_Pos]) --> [skillfull].
adj(adj(together),      [cat:adj,sem:together,        sound:hard,pos:_Pos]) --> [together].
adj(adj(top),           [cat:adj,sem:top,             sound:hard,pos:_Pos]) --> [top].
adj(adj(uncommon),      [cat:adj,sem:uncommon:1,      sound:hard,pos:_Pos]) --> [uncommon].
adj(adj(wonderful),     [cat:adj,sem:wonderful,       sound:hard,pos:_Pos]) --> [wonderful].

/* Size */

adj(adj(harder),    [cat:adj,sem:hard:1, sound:hard,pos:_Pos])   --> [harder].
adj(adj(large),     [cat:adj,sem:large:1, sound:hard,pos:_Pos])  --> [large].
adj(adj(larger),    [cat:adj,sem:large:1, sound:hard,pos:_Pos])  --> [larger].
adj(adj(largest),   [cat:adj,sem:large:1, sound:hard,pos:_Pos])  --> [largest].
adj(adj(more),      [cat:adj,sem:more:1,  sound:hard,pos:_Pos])  --> [more].
adj(adj(small),     [cat:adj,sem:small:1, sound:hard,pos:_Pos])  --> [small].
adj(adj(tiny),      [cat:adj,sem:tiny:1,  sound:hard,pos:_Pos])  --> [tiny].
adj(adj(little),    [cas:adj,sem:little:1,sound:hard,pos:_Pos])  --> [little].

/* Length */

adj(adj(long),[cat:adj,sem:long:1,sound:hard,pos:_Pos]) --> [long].

/* distance */

adj(adj(near),[cat:adj,sem:near:1,sound:hard,pos:_Pos]) --> [near].

/* Colours */

adj(adj(black), [cat:adj,sem:black:1, sound:hard,pos:_Pos]) --> [black].
adj(adj(blue),  [cat:adj,sem:blue:1,  sound:hard,pos:_Pos]) --> [blue].
adj(adj(gold),  [cat:adj,sem:gold:1,  sound:hard,pos:_Pos]) --> [gold].
adj(adj(orange),[cat:adj,sem:orange:1,sound:soft,pos:_Pos]) --> [orange].
adj(adj(yellow),[cat:adj,sem:yellow:1,sound:hard,pos:_Pos]) --> [yellow].

/* Order */

adj(adj(final), [cat:adj,sem:final:1, sound:hard,pos:_Pos]) --> [final].
adj(adj(first), [cat:adj,sem:first:1, sound:hard,pos:_Pos]) --> [first].
adj(adj(next),  [cat:adj,sem:next:1,  sound:hard,pos:_Pos]) --> [next].
adj(adj(other), [cat:adj,sem:other:1, sound:hard,pos:_Pos]) --> [other].
adj(adj(second),[cat:adj,sem:second:1,sound:hard,pos:_Pos]) --> [second].
adj(adj(third), [cat:adj,sem:third:1, sound:hard,pos:_Pos]) --> [third].


/* Intensifiers */

ints(ints(beautifully),[cat:adj,sem:beautiful:1,sound:hard,type:adj]) --> [beautifully].
ints(ints(very),       [cat:adj,sem:very:1     ,sound:hard,type:adj]) --> [very].


