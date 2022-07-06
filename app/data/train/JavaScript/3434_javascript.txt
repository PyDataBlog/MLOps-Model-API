import React from 'react';
import PostImage from '../components/story/PostImage';
import TwoPostImages from '../components/story/TwoPostImages';
import StoryPage from '../components/story/StoryPage';
import StoryTextBlock from '../components/story/StoryTextBlock';
import StoryImages from '../components/story/StoryImages';
import StoryIntro from '../components/story/StoryIntro';

const imgDirPath = "/images/stories/2016-11-20-irina-and-lucian-maternity-photo-session/";
const imgDirPath1 = "stories/2016-11-20-irina-and-lucian-maternity-photo-session";

class IrinaAndLucianMaternityPhotoSessionStory extends React.Component {

  constructor() {
    super();
  }

  render() {
    return (
      <StoryPage logoDirPath={imgDirPath1}
                 logoPrefix="teaser"
                 logoNumber="08-2048"
                 title="Irina & Lucian Maternity Photos"
                 author="Dan"
                 location="Charlottenburg Palace, Berlin"
                 tags="maternity, baby, pregnancy">

        <StoryIntro>
          Our friends, Lucian and Irina are having a baby. This is such a wonderful moment!
          We decided to go together at the Charlottenburg Palace to do a maternity photo session.
          We were really lucky we got a wonderful sunny weekend, the last sunny one this autumn,
          before all of the leaves have already fallen.
        </StoryIntro>

        <StoryTextBlock title="The Charlottenburg Palace">
          The impressive palace is a great place to take photos and this time is
          covered with scaffolding, which somehow gives a symbolic hint to our photo shoot:
          &quot;work in progress!&quot; We start our photo session here,
          taking advantage of the bright sun. I&apos;m using a polarizer filter here,
          to make the colors pop!
        </StoryTextBlock>

        <StoryImages>
          <PostImage dirPath={imgDirPath} number="01" />
          <TwoPostImages dirPath={imgDirPath}
                         number1="02"
                         number2="03" />
          <TwoPostImages dirPath={imgDirPath}
                         number1="05"
                         number2="06" />
          <PostImage dirPath={imgDirPath} number="07" />
          <PostImage dirPath={imgDirPath} number="10" />
          <PostImage dirPath={imgDirPath} number="27" />
        </StoryImages>

        <StoryTextBlock title="The Forest">
          The sun coming through the colorful leaves of the autumn gave the perfect light for the next photos.
          Using a long lens allowed me to isolate my subjects and bring that great shallow depth of field!
          I&apos;ll let you observe the authentic joy on Lucian and Irina&apos;s faces, brought by this great moment in their lives,
          having a baby.
        </StoryTextBlock>

        <StoryImages>
          <PostImage dirPath={imgDirPath} number="16" />
          <TwoPostImages dirPath={imgDirPath}
                         number1="13"
                         number2="15" />
          <PostImage dirPath={imgDirPath} number="19" />
          <TwoPostImages dirPath={imgDirPath}
                         number1="20"
                         number2="21" />
          <PostImage dirPath={imgDirPath} number="22" />
          <TwoPostImages dirPath={imgDirPath}
                         number1="23"
                         number2="24" />
          <PostImage dirPath={imgDirPath} number="35" />
        </StoryImages>

        <StoryTextBlock title="The Lake">
          Moving away from &quot;the forest&quot;, we chose the nearby lake is our third location.
          <br/>
          In the next photo, the contrast between the blue lake and Irina&apos;s orange
          pullover is just amazing.
          <br/>
          You might already know that the bridge in the Charlottenburg Garden is
          one of our <a href="/streets-of-berlin/charlottenburg-bridge-in-autumn.html">favorite
          places</a>. Like always, it gave me a really nice opportunity to play
          with reflections.
        </StoryTextBlock>

        <StoryImages>
          <PostImage dirPath={imgDirPath} number="28" />
          <PostImage dirPath={imgDirPath} number="30" />
          <PostImage dirPath={imgDirPath} number="31" />
          <PostImage dirPath={imgDirPath} number="37" />
          <PostImage dirPath={imgDirPath} number="39" />
          <PostImage dirPath={imgDirPath} number="42" />
        </StoryImages>

        <StoryTextBlock title="The Autumn">
          The colors of the autumn are just amazing! My favorite photo from this
          series is the next one. I call it &quot;The Tree of Life&quot;, as there are four
          colors spreading from each of the corners, one for each of the seasons.
          We have spring and summer to the left, and autumn and winter to the right.
          And of course, pregnant Irina in the middle getting support from her dear
          husband, Lucian.
        </StoryTextBlock>

        <StoryImages>
          <PostImage dirPath={imgDirPath} number="44" />
          <PostImage dirPath={imgDirPath} number="46" />
          <PostImage dirPath={imgDirPath} number="48" />
          <PostImage dirPath={imgDirPath} number="51" />
          <TwoPostImages dirPath={imgDirPath}
                         number1="52"
                         number2="53" />
          <PostImage dirPath={imgDirPath} number="54" />
          <TwoPostImages dirPath={imgDirPath}
                         number1="55"
                         number2="56" />
          <PostImage dirPath={imgDirPath} number="59" />
          <TwoPostImages dirPath={imgDirPath}
                         number1="60"
                         number2="61" />
          <TwoPostImages dirPath={imgDirPath}
                         number1="62"
                         number2="63" />
          <PostImage dirPath={imgDirPath} number="64" />
        </StoryImages>
      </StoryPage>);
  }
}

export default IrinaAndLucianMaternityPhotoSessionStory;
