import React from 'react';
import PropTypes from 'prop-types';
import Text from '../Text/Text';
import { Styled } from './CalloutGroupItem.styles';

const CalloutGroupItem = ({ text, imgSrc, imgAlt, emoji }) => {
	return (
		<>
			{!imgSrc ? (
				!emoji ? (
					<Text>{text}</Text>
				) : (
					<Text>
						<Styled.CalloutGroupItemTool>
							<Styled.CalloutGroupItemEmoji role="img"
								aria-label={imgAlt}>
								{emoji}
							</Styled.CalloutGroupItemEmoji>
							{text}
						</Styled.CalloutGroupItemTool>
					</Text>
				)
			) : (
				<Text>
					<Styled.CalloutGroupItemTool>
						<Styled.CalloutGroupItemLogo src={imgSrc}
							alt={imgAlt}
							aria-label={imgAlt} />
						{text}
					</Styled.CalloutGroupItemTool>
				</Text>
			)}
		</>
	);
};

CalloutGroupItem.defaultProps = {
	imgSrc: null,
	imgAlt: null,
	emoji: null,
};

CalloutGroupItem.propTypes = {
	emoji: PropTypes.string,
	text: PropTypes.oneOfType([PropTypes.node, PropTypes.string]).isRequired,
	imgSrc: PropTypes.node,
	imgAlt: PropTypes.string,
};

export default CalloutGroupItem;
