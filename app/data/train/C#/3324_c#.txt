using System;
using UnityCMF.ECore;

namespace UnityCMF.CCore
{
	public interface CPackage
	{
		CFactory FactoryInstance { get; }

		EClassifier getClassifier(string name);
	}
}

