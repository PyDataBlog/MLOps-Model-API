using System;
using System.Collections.Generic;
using System.Reactive.Linq;
using System.Reactive.Threading.Tasks;
using MS.Core;

namespace System.Security.Cryptography
{
    public static class __SHA512Managed
    {
        public static IObservable<System.Reactive.Unit> Initialize(
            this IObservable<System.Security.Cryptography.SHA512Managed> SHA512ManagedValue)
        {
            return
                Observable.Do(SHA512ManagedValue, (SHA512ManagedValueLambda) => SHA512ManagedValueLambda.Initialize())
                    .ToUnit();
        }
    }
}