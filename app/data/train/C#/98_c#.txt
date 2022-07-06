using System;
using System.Collections.Generic;
using System.Reactive.Linq;
using System.Reactive.Threading.Tasks;
using MS.Core;

namespace System.Runtime.Remoting.Contexts
{
    public static class __ContextAttribute
    {
        public static IObservable<System.Boolean> IsNewContextOK(
            this IObservable<System.Runtime.Remoting.Contexts.ContextAttribute> ContextAttributeValue,
            IObservable<System.Runtime.Remoting.Contexts.Context> newCtx)
        {
            return Observable.Zip(ContextAttributeValue, newCtx,
                (ContextAttributeValueLambda, newCtxLambda) => ContextAttributeValueLambda.IsNewContextOK(newCtxLambda));
        }


        public static IObservable<System.Reactive.Unit> Freeze(
            this IObservable<System.Runtime.Remoting.Contexts.ContextAttribute> ContextAttributeValue,
            IObservable<System.Runtime.Remoting.Contexts.Context> newContext)
        {
            return ObservableExt.ZipExecute(ContextAttributeValue, newContext,
                (ContextAttributeValueLambda, newContextLambda) => ContextAttributeValueLambda.Freeze(newContextLambda));
        }


        public static IObservable<System.Boolean> Equals(
            this IObservable<System.Runtime.Remoting.Contexts.ContextAttribute> ContextAttributeValue,
            IObservable<System.Object> o)
        {
            return Observable.Zip(ContextAttributeValue, o,
                (ContextAttributeValueLambda, oLambda) => ContextAttributeValueLambda.Equals(oLambda));
        }


        public static IObservable<System.Int32> GetHashCode(
            this IObservable<System.Runtime.Remoting.Contexts.ContextAttribute> ContextAttributeValue)
        {
            return Observable.Select(ContextAttributeValue,
                (ContextAttributeValueLambda) => ContextAttributeValueLambda.GetHashCode());
        }


        public static IObservable<System.Boolean> IsContextOK(
            this IObservable<System.Runtime.Remoting.Contexts.ContextAttribute> ContextAttributeValue,
            IObservable<System.Runtime.Remoting.Contexts.Context> ctx,
            IObservable<System.Runtime.Remoting.Activation.IConstructionCallMessage> ctorMsg)
        {
            return Observable.Zip(ContextAttributeValue, ctx, ctorMsg,
                (ContextAttributeValueLambda, ctxLambda, ctorMsgLambda) =>
                    ContextAttributeValueLambda.IsContextOK(ctxLambda, ctorMsgLambda));
        }


        public static IObservable<System.Reactive.Unit> GetPropertiesForNewContext(
            this IObservable<System.Runtime.Remoting.Contexts.ContextAttribute> ContextAttributeValue,
            IObservable<System.Runtime.Remoting.Activation.IConstructionCallMessage> ctorMsg)
        {
            return ObservableExt.ZipExecute(ContextAttributeValue, ctorMsg,
                (ContextAttributeValueLambda, ctorMsgLambda) =>
                    ContextAttributeValueLambda.GetPropertiesForNewContext(ctorMsgLambda));
        }


        public static IObservable<System.String> get_Name(
            this IObservable<System.Runtime.Remoting.Contexts.ContextAttribute> ContextAttributeValue)
        {
            return Observable.Select(ContextAttributeValue,
                (ContextAttributeValueLambda) => ContextAttributeValueLambda.Name);
        }
    }
}