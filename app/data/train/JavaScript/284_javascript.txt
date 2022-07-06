import React from "react";
import styled from 'styled-components'
import Link from './link';

const nextArrow = "/icons/next-arrow.png";
const prevArrow = "/icons/prev-arrow.png";

const PatternLink = styled.span`
  width: 100%;
  display: flex;
  flex-direction: column;
  padding: 1em;
  float: ${props => props.previous ? 'left' : 'right'}

  @media(min-width: $width-tablet) {
    width: auto;
  }
`;

const ImageContainer = styled.span`
  height: 50px;
`;

const Image = styled.img`
  height: 100%;
  background-color: white;
  float: ${props => props.previous ? 'right' : 'left'}
`;

const ArrowContainer = styled.div`
  display: flex;
  flex-direction: ${props => props.previous ? 'row-reverse' : 'row'};
  align-items: center;
`;

const Name = styled.p`
  padding: 10px 0;
`;

const Arrow = styled.img`
  height: 10px;
  flex-direction: row-reverse;
  padding:  ${props => props.previous ? '0 10px 0 0' : '0 0 0 10px'};
`;

const NextPrevPattern = ({pattern, direction}) => {
  const previous = direction === "previous"

  return (
    <Link href={pattern.url}>
      <PatternLink previous={previous}>
        <ImageContainer>
          <Image previous={previous} src={pattern.painted || pattern.lineDrawing} />
        </ImageContainer>
        <ArrowContainer previous={previous}>
          <Name>{pattern.name}</Name>
          {
            (direction === "next") &&
            <Arrow src={nextArrow}/>
          }
          {
            (direction === "previous") &&
            <Arrow previous src={prevArrow} />
          }
        </ArrowContainer>
      </PatternLink>
    </Link>
  )
};

export default NextPrevPattern;
