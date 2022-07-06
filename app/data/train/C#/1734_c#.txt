using System.Drawing;

namespace GVNET
{
	/// <summary>
	/// Basic node with added functionality to control the shape, color, and style of the node.
	/// </summary>
	public interface INodeSimple : INode
	{
		/// <summary>
		/// Set the shape of a node.
		/// </summary>
		[DOTKeyword("shape")]
		Shape NodeShape { get; }

		/// <summary>
		/// Color used to fill the background of a node assuming Style=Filled.
		/// If this is not defined, the default is used, except for shape=point or when the output format is MIF, which use black by default.
		/// </summary>
		[DOTKeyword("color")]
		Color Color { get; }

		/// <summary>
		/// Sets the style of the node.
		/// </summary>
		[DOTKeyword("style")]
		NodeStyle Style { get; }

		/// <summary>
		/// Sets the style of the border of the node.
		/// </summary>
		[DOTKeyword("style")]
		BorderStyle BorderStyle { get; }
	}
}
