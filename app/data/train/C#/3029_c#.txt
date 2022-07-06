// Decompiled with JetBrains decompiler
// Type: LinqToWiki.Generated.Entities.categorymembersWhere
// Assembly: LinqToWiki.Generated, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null
// MVID: A4304DCD-2C3F-4D9F-B552-69913FDA3067
// Assembly location: D:\work\my\AsoiafWikiDb\AsoiafWikiDb\bin\LinqToWiki.Generated.dll

using LinqToWiki;
using LinqToWiki.Collections;
using LinqToWiki.Generated;
using System;

namespace LinqToWiki.Generated.Entities
{
  public sealed class categorymembersWhere
  {
    /// <summary>
    /// Which category to enumerate (required). Must include Category: prefix. Cannot be used together with cmpageid
    /// 
    /// </summary>
    public string title { get; private set; }

    /// <summary>
    /// Page ID of the category to enumerate. Cannot be used together with cmtitle
    /// 
    /// </summary>
    public long pageid { get; private set; }

    /// <summary>
    /// Only include pages in these namespaces
    ///             NOTE: Due to $wgMiserMode, using this may result in fewer than "cmlimit" results
    ///             returned before continuing; in extreme cases, zero results may be returned.
    ///             Note that you can use cmtype=subcat or cmtype=file instead of cmnamespace=14 or 6.
    /// 
    /// </summary>
    public ItemOrCollection<Namespace> ns { get; private set; }

    /// <summary>
    /// What type of category members to include. Ignored when cmsort=timestamp is set
    /// 
    /// </summary>
    public ItemOrCollection<categorymemberstype> type { get; private set; }

    /// <summary>
    /// Timestamp to start listing from. Can only be used with cmsort=timestamp
    /// 
    /// </summary>
    public DateTime start { get; private set; }

    /// <summary>
    /// Timestamp to end listing at. Can only be used with cmsort=timestamp
    /// 
    /// </summary>
    public DateTime end { get; private set; }

    /// <summary>
    /// Sortkey to start listing from. Must be given in binary format. Can only be used with cmsort=sortkey
    /// 
    /// </summary>
    public string startsortkey { get; private set; }

    /// <summary>
    /// Sortkey to end listing at. Must be given in binary format. Can only be used with cmsort=sortkey
    /// 
    /// </summary>
    public string endsortkey { get; private set; }

    /// <summary>
    /// Sortkey prefix to start listing from. Can only be used with cmsort=sortkey. Overrides cmstartsortkey
    /// 
    /// </summary>
    public string startsortkeyprefix { get; private set; }

    /// <summary>
    /// Sortkey prefix to end listing BEFORE (not at, if this value occurs it will not be included!). Can only be used with cmsort=sortkey. Overrides cmendsortkey
    /// 
    /// </summary>
    public string endsortkeyprefix { get; private set; }

    private categorymembersWhere()
    {
    }
  }
}
