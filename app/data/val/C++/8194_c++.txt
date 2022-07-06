/* 
 * Copyright (C) 2013 Cupertino Miranda
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see <http://www.gnu.org/
 * licenses>.
 * 
 * Contributors:
 *     Cupertino Miranda - initial API and implementation
 */
#include "LabelCell.h"
#include "../Core/fonts/courB08.h"

//bool LabelCell::startAnimation()
//{
//	printf("Entering animation\n");
//	FontAccess font(font_courB08);
//	SpecialFontTexture texture(&font);
//	texture.setProgString(getData()->string_pointer);
//	if(selected && texture.width() > frame.size.width)
//		return true;
//	return false;
//}
//bool LabelCell::endAnimation() {
//	if(selected)
//		return false;
//}
void LabelCell::draw()
{
	int16_t pos = 0;

	FontAccess font(font_courB08);
	SpecialFontTexture texture(&font);
	texture.setProgString(getData()->string_pointer);

	if(0 && selected && texture.width() > frame.size.width)
	{
		uint8_t diff = texture.width() - frame.size.width;
		Time step = animationStep();
		const uint8_t slide_speed = 1;

		pos = -(step * slide_speed);
		if(step * slide_speed < diff)
			pos = -diff + ((step * slide_speed) - diff);


		printf("STEP = %d, POS = %d\n", step, pos);
	}

	View::draw_with(pos, 0, &texture);
}

