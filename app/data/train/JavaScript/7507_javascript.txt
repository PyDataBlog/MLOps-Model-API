import React from "react";
import styled from "styled-components";
import Img from "gatsby-image";
import { graphql } from "gatsby";
import Center from "../components/center";
import ResponsiveContainer from "../components/responsive-container";
import Inset from "../components/inset";
import Text from "../components/text";
import colors from "../constants/colors";
import Layout from "../layouts";
import VersandhausWalzLogo from "../assets/compressed/versandhaus-walz.png";
import OttoLogo from "../assets/compressed/otto.png";
import LudwigMediaLogo from "../assets/compressed/ludwigmedia.png";
import BadischerWeinLogo from "../assets/compressed/badischer-wein.png";
import DrawkitContentManColour from "../assets/drawkit-content-man-colour.svg";
import Stack from "../components/stack";

const customers = [
  {
    src: OttoLogo,
    alt: "Otto (GmbH & CoKG)"
  },
  {
    src: VersandhausWalzLogo,
    alt: "Versandhaus Walz"
  },
  {
    src: LudwigMediaLogo,
    alt: "LUDWIG:media gmbh"
  },
  {
    src: BadischerWeinLogo,
    alt: "Badischer Wein eKfr"
  }
];

const HeroImage = styled.img`
  width: 80%;
  height: auto;
  @media (min-width: 48em) {
    width: auto;
    height: 80vh;
    max-height: 460px;
    margin-left: -32px;
  }
`;

const HeroLayout = styled(Stack)`
  margin: 64px 0 32px;
  @media (min-width: 48em) {
    margin-top: 0;
    flex-direction: row;
  }
`;

const HeroArea = styled.div`
  position: relative;
  background-color: ${colors.black};
  display: flex;
  align-items: center;
  @media (min-width: 48em) {
    height: 80vh;
  }
`;

const HeroTextArea = styled.div`
  position: relative;
`;

const HeroText = styled.h1`
  line-height: 1.1;
  font-size: 2.5rem;
  max-width: 22ch;
  margin-top: 0;
  color: ${colors.white};
  text-align: center;
  @media (min-width: 48em) {
    font-size: 3rem;
    text-align: left;
  }
`;

const HeroTextHighlicht = styled.span`
  color: ${colors.orange};
`;

const HeroTextSubheadline = styled.p`
  line-height: 1.1;
  font-size: 1.6rem;
  max-width: 22ch;
  color: #ddb992;
  margin: 0;
  text-align: center;
  @media (min-width: 48em) {
    text-align: left;
    font-size: 2rem;
  }
`;

const CustomerContainer = ResponsiveContainer.extend`
  display: flex;
  align-items: center;
  flex-direction: column;
  @media (min-width: 700px) {
    flex-direction: row;
  }
`;

const CustomerList = Center.extend`
  padding: 32px 0;
  background-color: hsla(31, 17%, 93%, 1);
`;

const CustomerLogo = styled.div`
  flex: 1 1 auto;
  max-height: 60px;
  margin: 10px;
  transition: all 0.5s ease-in-out;

  > img {
    max-width: 100%;
    max-height: 100%;
  }
`;

const H2 = styled.h2`
  color: ${colors.textPrimary};
  margin: 0 0 24px;
`;
const H3 = styled.h3`
  color: ${colors.textPrimary};
  margin: 0 0 16px;
`;

const SkillGrid = styled.div`
  display: grid;
  grid-template-columns: 1fr;
  grid-gap: 16px;
  @media (min-width: 700px) {
    grid-template-columns: 1fr 1fr;
  }
`;

const ServiceSection = styled.div`
  background-color: hsla(13, 10%, 97%, 1);
  padding: 32px 0;
`;

const Skill = styled.div`
  background-color: white;
  border-radius: 4px;

  display: flex;
  flex-direction: column;

  text-align: left;
  color: ${colors.text};
  animation: fadein 2s;
`;

const SkillImage = styled.div`
  max-height: 175px;
  background-color: #ffe6df;
  border-radius: 4px 4px 0 0;
  overflow: hidden;
`;

const IndexPage = ({ data }) => (
  <Layout>
    <HeroArea>
      <ResponsiveContainer>
        <HeroLayout alignItems="center" scale="xl">
          <HeroTextArea>
            <HeroText>
              Wir liefern digitale Lösungen,<br />
              <HeroTextHighlicht>die unsere Kunden lieben.</HeroTextHighlicht>
            </HeroText>
            <HeroTextSubheadline>Und das seit 15 Jahren!</HeroTextSubheadline>
          </HeroTextArea>
          <HeroImage src={DrawkitContentManColour} alt="" />
        </HeroLayout>
      </ResponsiveContainer>
    </HeroArea>
    <CustomerList>
      <CustomerContainer>
        {customers.map(logo => (
          <CustomerLogo key={logo.alt}>
            <img src={logo.src} alt={logo.alt} />
          </CustomerLogo>
        ))}
      </CustomerContainer>
    </CustomerList>
    <ServiceSection>
      <ResponsiveContainer>
        <Center>
          <H2>Unsere Leistungen</H2>
        </Center>
        <SkillGrid>
          {data.allContentfulServices.edges.map(({ node }) => (
            <Skill key={node.id}>
              <SkillImage>
                {node.image && <Img fluid={node.image.fluid} />}
              </SkillImage>
              <Inset scale="xl">
                <H3>{node.title}</H3>
                <Text.Detail>
                  <div
                    dangerouslySetInnerHTML={{
                      __html: node.description.childMarkdownRemark.html
                    }}
                  />
                </Text.Detail>
              </Inset>
            </Skill>
          ))}
        </SkillGrid>
      </ResponsiveContainer>
    </ServiceSection>
  </Layout>
);

export default IndexPage;

export const query = graphql`
  query AllServices {
    allContentfulServices(sort: { fields: [order], order: ASC }) {
      edges {
        node {
          id
          title
          description {
            childMarkdownRemark {
              html
            }
          }
          image {
            fluid(maxWidth: 426) {
              ...GatsbyContentfulFluid
            }
          }
        }
      }
    }
    contentfulAsset(title: { eq: "Hero" }) {
      fluid(maxHeight: 1000) {
        ...GatsbyContentfulFluid
      }
    }
  }
`;
