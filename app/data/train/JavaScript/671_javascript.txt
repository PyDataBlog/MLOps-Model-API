// Compiled by ClojureScript 1.7.170 {}
goog.provide('figwheel.client.file_reloading');
goog.require('cljs.core');
goog.require('goog.string');
goog.require('goog.Uri');
goog.require('goog.net.jsloader');
goog.require('cljs.core.async');
goog.require('goog.object');
goog.require('clojure.set');
goog.require('clojure.string');
goog.require('figwheel.client.utils');
figwheel.client.file_reloading.queued_file_reload;
if(typeof figwheel.client.file_reloading.figwheel_meta_pragmas !== 'undefined'){
} else {
figwheel.client.file_reloading.figwheel_meta_pragmas = cljs.core.atom.call(null,cljs.core.PersistentArrayMap.EMPTY);
}
figwheel.client.file_reloading.on_jsload_custom_event = (function figwheel$client$file_reloading$on_jsload_custom_event(url){
return figwheel.client.utils.dispatch_custom_event.call(null,"figwheel.js-reload",url);
});
figwheel.client.file_reloading.before_jsload_custom_event = (function figwheel$client$file_reloading$before_jsload_custom_event(files){
return figwheel.client.utils.dispatch_custom_event.call(null,"figwheel.before-js-reload",files);
});
figwheel.client.file_reloading.namespace_file_map_QMARK_ = (function figwheel$client$file_reloading$namespace_file_map_QMARK_(m){
var or__16826__auto__ = (cljs.core.map_QMARK_.call(null,m)) && (typeof new cljs.core.Keyword(null,"namespace","namespace",-377510372).cljs$core$IFn$_invoke$arity$1(m) === 'string') && (((new cljs.core.Keyword(null,"file","file",-1269645878).cljs$core$IFn$_invoke$arity$1(m) == null)) || (typeof new cljs.core.Keyword(null,"file","file",-1269645878).cljs$core$IFn$_invoke$arity$1(m) === 'string')) && (cljs.core._EQ_.call(null,new cljs.core.Keyword(null,"type","type",1174270348).cljs$core$IFn$_invoke$arity$1(m),new cljs.core.Keyword(null,"namespace","namespace",-377510372)));
if(or__16826__auto__){
return or__16826__auto__;
} else {
cljs.core.println.call(null,"Error not namespace-file-map",cljs.core.pr_str.call(null,m));

return false;
}
});
figwheel.client.file_reloading.add_cache_buster = (function figwheel$client$file_reloading$add_cache_buster(url){

return goog.Uri.parse(url).makeUnique();
});
figwheel.client.file_reloading.name__GT_path = (function figwheel$client$file_reloading$name__GT_path(ns){

return (goog.dependencies_.nameToPath[ns]);
});
figwheel.client.file_reloading.provided_QMARK_ = (function figwheel$client$file_reloading$provided_QMARK_(ns){
return (goog.dependencies_.written[figwheel.client.file_reloading.name__GT_path.call(null,ns)]);
});
figwheel.client.file_reloading.fix_node_request_url = (function figwheel$client$file_reloading$fix_node_request_url(url){

if(cljs.core.truth_(goog.string.startsWith(url,"../"))){
return clojure.string.replace.call(null,url,"../","");
} else {
return [cljs.core.str("goog/"),cljs.core.str(url)].join('');
}
});
figwheel.client.file_reloading.immutable_ns_QMARK_ = (function figwheel$client$file_reloading$immutable_ns_QMARK_(name){
var or__16826__auto__ = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 9, ["svgpan.SvgPan",null,"far.out",null,"testDep.bar",null,"someprotopackage.TestPackageTypes",null,"goog",null,"an.existing.path",null,"cljs.core",null,"ns",null,"dup.base",null], null), null).call(null,name);
if(cljs.core.truth_(or__16826__auto__)){
return or__16826__auto__;
} else {
return cljs.core.some.call(null,cljs.core.partial.call(null,goog.string.startsWith,name),new cljs.core.PersistentVector(null, 5, 5, cljs.core.PersistentVector.EMPTY_NODE, ["goog.","cljs.","clojure.","fake.","proto2."], null));
}
});
figwheel.client.file_reloading.get_requires = (function figwheel$client$file_reloading$get_requires(ns){
return cljs.core.set.call(null,cljs.core.filter.call(null,(function (p1__23844_SHARP_){
return cljs.core.not.call(null,figwheel.client.file_reloading.immutable_ns_QMARK_.call(null,p1__23844_SHARP_));
}),goog.object.getKeys((goog.dependencies_.requires[figwheel.client.file_reloading.name__GT_path.call(null,ns)]))));
});
if(typeof figwheel.client.file_reloading.dependency_data !== 'undefined'){
} else {
figwheel.client.file_reloading.dependency_data = cljs.core.atom.call(null,new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"pathToName","pathToName",-1236616181),cljs.core.PersistentArrayMap.EMPTY,new cljs.core.Keyword(null,"dependents","dependents",136812837),cljs.core.PersistentArrayMap.EMPTY], null));
}
figwheel.client.file_reloading.path_to_name_BANG_ = (function figwheel$client$file_reloading$path_to_name_BANG_(path,name){
return cljs.core.swap_BANG_.call(null,figwheel.client.file_reloading.dependency_data,cljs.core.update_in,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"pathToName","pathToName",-1236616181),path], null),cljs.core.fnil.call(null,clojure.set.union,cljs.core.PersistentHashSet.EMPTY),cljs.core.PersistentHashSet.fromArray([name], true));
});
/**
 * Setup a path to name dependencies map.
 * That goes from path -> #{ ns-names }
 */
figwheel.client.file_reloading.setup_path__GT_name_BANG_ = (function figwheel$client$file_reloading$setup_path__GT_name_BANG_(){
var nameToPath = goog.object.filter(goog.dependencies_.nameToPath,(function (v,k,o){
return goog.string.startsWith(v,"../");
}));
return goog.object.forEach(nameToPath,((function (nameToPath){
return (function (v,k,o){
return figwheel.client.file_reloading.path_to_name_BANG_.call(null,v,k);
});})(nameToPath))
);
});
/**
 * returns a set of namespaces defined by a path
 */
figwheel.client.file_reloading.path__GT_name = (function figwheel$client$file_reloading$path__GT_name(path){
return cljs.core.get_in.call(null,cljs.core.deref.call(null,figwheel.client.file_reloading.dependency_data),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"pathToName","pathToName",-1236616181),path], null));
});
figwheel.client.file_reloading.name_to_parent_BANG_ = (function figwheel$client$file_reloading$name_to_parent_BANG_(ns,parent_ns){
return cljs.core.swap_BANG_.call(null,figwheel.client.file_reloading.dependency_data,cljs.core.update_in,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"dependents","dependents",136812837),ns], null),cljs.core.fnil.call(null,clojure.set.union,cljs.core.PersistentHashSet.EMPTY),cljs.core.PersistentHashSet.fromArray([parent_ns], true));
});
/**
 * This reverses the goog.dependencies_.requires for looking up ns-dependents.
 */
figwheel.client.file_reloading.setup_ns__GT_dependents_BANG_ = (function figwheel$client$file_reloading$setup_ns__GT_dependents_BANG_(){
var requires = goog.object.filter(goog.dependencies_.requires,(function (v,k,o){
return goog.string.startsWith(k,"../");
}));
return goog.object.forEach(requires,((function (requires){
return (function (v,k,_){
return goog.object.forEach(v,((function (requires){
return (function (v_SINGLEQUOTE_,k_SINGLEQUOTE_,___$1){
var seq__23849 = cljs.core.seq.call(null,figwheel.client.file_reloading.path__GT_name.call(null,k));
var chunk__23850 = null;
var count__23851 = (0);
var i__23852 = (0);
while(true){
if((i__23852 < count__23851)){
var n = cljs.core._nth.call(null,chunk__23850,i__23852);
figwheel.client.file_reloading.name_to_parent_BANG_.call(null,k_SINGLEQUOTE_,n);

var G__23853 = seq__23849;
var G__23854 = chunk__23850;
var G__23855 = count__23851;
var G__23856 = (i__23852 + (1));
seq__23849 = G__23853;
chunk__23850 = G__23854;
count__23851 = G__23855;
i__23852 = G__23856;
continue;
} else {
var temp__4425__auto__ = cljs.core.seq.call(null,seq__23849);
if(temp__4425__auto__){
var seq__23849__$1 = temp__4425__auto__;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__23849__$1)){
var c__17629__auto__ = cljs.core.chunk_first.call(null,seq__23849__$1);
var G__23857 = cljs.core.chunk_rest.call(null,seq__23849__$1);
var G__23858 = c__17629__auto__;
var G__23859 = cljs.core.count.call(null,c__17629__auto__);
var G__23860 = (0);
seq__23849 = G__23857;
chunk__23850 = G__23858;
count__23851 = G__23859;
i__23852 = G__23860;
continue;
} else {
var n = cljs.core.first.call(null,seq__23849__$1);
figwheel.client.file_reloading.name_to_parent_BANG_.call(null,k_SINGLEQUOTE_,n);

var G__23861 = cljs.core.next.call(null,seq__23849__$1);
var G__23862 = null;
var G__23863 = (0);
var G__23864 = (0);
seq__23849 = G__23861;
chunk__23850 = G__23862;
count__23851 = G__23863;
i__23852 = G__23864;
continue;
}
} else {
return null;
}
}
break;
}
});})(requires))
);
});})(requires))
);
});
figwheel.client.file_reloading.ns__GT_dependents = (function figwheel$client$file_reloading$ns__GT_dependents(ns){
return cljs.core.get_in.call(null,cljs.core.deref.call(null,figwheel.client.file_reloading.dependency_data),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"dependents","dependents",136812837),ns], null));
});
figwheel.client.file_reloading.build_topo_sort = (function figwheel$client$file_reloading$build_topo_sort(get_deps){
var get_deps__$1 = cljs.core.memoize.call(null,get_deps);
var topo_sort_helper_STAR_ = ((function (get_deps__$1){
return (function figwheel$client$file_reloading$build_topo_sort_$_topo_sort_helper_STAR_(x,depth,state){
var deps = get_deps__$1.call(null,x);
if(cljs.core.empty_QMARK_.call(null,deps)){
return null;
} else {
return topo_sort_STAR_.call(null,deps,depth,state);
}
});})(get_deps__$1))
;
var topo_sort_STAR_ = ((function (get_deps__$1){
return (function() {
var figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR_ = null;
var figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR___1 = (function (deps){
return figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR_.call(null,deps,(0),cljs.core.atom.call(null,cljs.core.sorted_map.call(null)));
});
var figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR___3 = (function (deps,depth,state){
cljs.core.swap_BANG_.call(null,state,cljs.core.update_in,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [depth], null),cljs.core.fnil.call(null,cljs.core.into,cljs.core.PersistentHashSet.EMPTY),deps);

var seq__23903_23910 = cljs.core.seq.call(null,deps);
var chunk__23904_23911 = null;
var count__23905_23912 = (0);
var i__23906_23913 = (0);
while(true){
if((i__23906_23913 < count__23905_23912)){
var dep_23914 = cljs.core._nth.call(null,chunk__23904_23911,i__23906_23913);
topo_sort_helper_STAR_.call(null,dep_23914,(depth + (1)),state);

var G__23915 = seq__23903_23910;
var G__23916 = chunk__23904_23911;
var G__23917 = count__23905_23912;
var G__23918 = (i__23906_23913 + (1));
seq__23903_23910 = G__23915;
chunk__23904_23911 = G__23916;
count__23905_23912 = G__23917;
i__23906_23913 = G__23918;
continue;
} else {
var temp__4425__auto___23919 = cljs.core.seq.call(null,seq__23903_23910);
if(temp__4425__auto___23919){
var seq__23903_23920__$1 = temp__4425__auto___23919;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__23903_23920__$1)){
var c__17629__auto___23921 = cljs.core.chunk_first.call(null,seq__23903_23920__$1);
var G__23922 = cljs.core.chunk_rest.call(null,seq__23903_23920__$1);
var G__23923 = c__17629__auto___23921;
var G__23924 = cljs.core.count.call(null,c__17629__auto___23921);
var G__23925 = (0);
seq__23903_23910 = G__23922;
chunk__23904_23911 = G__23923;
count__23905_23912 = G__23924;
i__23906_23913 = G__23925;
continue;
} else {
var dep_23926 = cljs.core.first.call(null,seq__23903_23920__$1);
topo_sort_helper_STAR_.call(null,dep_23926,(depth + (1)),state);

var G__23927 = cljs.core.next.call(null,seq__23903_23920__$1);
var G__23928 = null;
var G__23929 = (0);
var G__23930 = (0);
seq__23903_23910 = G__23927;
chunk__23904_23911 = G__23928;
count__23905_23912 = G__23929;
i__23906_23913 = G__23930;
continue;
}
} else {
}
}
break;
}

if(cljs.core._EQ_.call(null,depth,(0))){
return elim_dups_STAR_.call(null,cljs.core.reverse.call(null,cljs.core.vals.call(null,cljs.core.deref.call(null,state))));
} else {
return null;
}
});
figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR_ = function(deps,depth,state){
switch(arguments.length){
case 1:
return figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR___1.call(this,deps);
case 3:
return figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR___3.call(this,deps,depth,state);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR_.cljs$core$IFn$_invoke$arity$1 = figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR___1;
figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR_.cljs$core$IFn$_invoke$arity$3 = figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR___3;
return figwheel$client$file_reloading$build_topo_sort_$_topo_sort_STAR_;
})()
;})(get_deps__$1))
;
var elim_dups_STAR_ = ((function (get_deps__$1){
return (function figwheel$client$file_reloading$build_topo_sort_$_elim_dups_STAR_(p__23907){
var vec__23909 = p__23907;
var x = cljs.core.nth.call(null,vec__23909,(0),null);
var xs = cljs.core.nthnext.call(null,vec__23909,(1));
if((x == null)){
return cljs.core.List.EMPTY;
} else {
return cljs.core.cons.call(null,x,figwheel$client$file_reloading$build_topo_sort_$_elim_dups_STAR_.call(null,cljs.core.map.call(null,((function (vec__23909,x,xs,get_deps__$1){
return (function (p1__23865_SHARP_){
return clojure.set.difference.call(null,p1__23865_SHARP_,x);
});})(vec__23909,x,xs,get_deps__$1))
,xs)));
}
});})(get_deps__$1))
;
return topo_sort_STAR_;
});
figwheel.client.file_reloading.get_all_dependencies = (function figwheel$client$file_reloading$get_all_dependencies(ns){
var topo_sort_SINGLEQUOTE_ = figwheel.client.file_reloading.build_topo_sort.call(null,figwheel.client.file_reloading.get_requires);
return cljs.core.apply.call(null,cljs.core.concat,topo_sort_SINGLEQUOTE_.call(null,cljs.core.set.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [ns], null))));
});
figwheel.client.file_reloading.get_all_dependents = (function figwheel$client$file_reloading$get_all_dependents(nss){
var topo_sort_SINGLEQUOTE_ = figwheel.client.file_reloading.build_topo_sort.call(null,figwheel.client.file_reloading.ns__GT_dependents);
return cljs.core.reverse.call(null,cljs.core.apply.call(null,cljs.core.concat,topo_sort_SINGLEQUOTE_.call(null,cljs.core.set.call(null,nss))));
});
figwheel.client.file_reloading.unprovide_BANG_ = (function figwheel$client$file_reloading$unprovide_BANG_(ns){
var path = figwheel.client.file_reloading.name__GT_path.call(null,ns);
goog.object.remove(goog.dependencies_.visited,path);

goog.object.remove(goog.dependencies_.written,path);

return goog.object.remove(goog.dependencies_.written,[cljs.core.str(goog.basePath),cljs.core.str(path)].join(''));
});
figwheel.client.file_reloading.resolve_ns = (function figwheel$client$file_reloading$resolve_ns(ns){
return [cljs.core.str(goog.basePath),cljs.core.str(figwheel.client.file_reloading.name__GT_path.call(null,ns))].join('');
});
figwheel.client.file_reloading.addDependency = (function figwheel$client$file_reloading$addDependency(path,provides,requires){
var seq__23943 = cljs.core.seq.call(null,provides);
var chunk__23944 = null;
var count__23945 = (0);
var i__23946 = (0);
while(true){
if((i__23946 < count__23945)){
var prov = cljs.core._nth.call(null,chunk__23944,i__23946);
figwheel.client.file_reloading.path_to_name_BANG_.call(null,path,prov);

var seq__23947_23955 = cljs.core.seq.call(null,requires);
var chunk__23948_23956 = null;
var count__23949_23957 = (0);
var i__23950_23958 = (0);
while(true){
if((i__23950_23958 < count__23949_23957)){
var req_23959 = cljs.core._nth.call(null,chunk__23948_23956,i__23950_23958);
figwheel.client.file_reloading.name_to_parent_BANG_.call(null,req_23959,prov);

var G__23960 = seq__23947_23955;
var G__23961 = chunk__23948_23956;
var G__23962 = count__23949_23957;
var G__23963 = (i__23950_23958 + (1));
seq__23947_23955 = G__23960;
chunk__23948_23956 = G__23961;
count__23949_23957 = G__23962;
i__23950_23958 = G__23963;
continue;
} else {
var temp__4425__auto___23964 = cljs.core.seq.call(null,seq__23947_23955);
if(temp__4425__auto___23964){
var seq__23947_23965__$1 = temp__4425__auto___23964;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__23947_23965__$1)){
var c__17629__auto___23966 = cljs.core.chunk_first.call(null,seq__23947_23965__$1);
var G__23967 = cljs.core.chunk_rest.call(null,seq__23947_23965__$1);
var G__23968 = c__17629__auto___23966;
var G__23969 = cljs.core.count.call(null,c__17629__auto___23966);
var G__23970 = (0);
seq__23947_23955 = G__23967;
chunk__23948_23956 = G__23968;
count__23949_23957 = G__23969;
i__23950_23958 = G__23970;
continue;
} else {
var req_23971 = cljs.core.first.call(null,seq__23947_23965__$1);
figwheel.client.file_reloading.name_to_parent_BANG_.call(null,req_23971,prov);

var G__23972 = cljs.core.next.call(null,seq__23947_23965__$1);
var G__23973 = null;
var G__23974 = (0);
var G__23975 = (0);
seq__23947_23955 = G__23972;
chunk__23948_23956 = G__23973;
count__23949_23957 = G__23974;
i__23950_23958 = G__23975;
continue;
}
} else {
}
}
break;
}

var G__23976 = seq__23943;
var G__23977 = chunk__23944;
var G__23978 = count__23945;
var G__23979 = (i__23946 + (1));
seq__23943 = G__23976;
chunk__23944 = G__23977;
count__23945 = G__23978;
i__23946 = G__23979;
continue;
} else {
var temp__4425__auto__ = cljs.core.seq.call(null,seq__23943);
if(temp__4425__auto__){
var seq__23943__$1 = temp__4425__auto__;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__23943__$1)){
var c__17629__auto__ = cljs.core.chunk_first.call(null,seq__23943__$1);
var G__23980 = cljs.core.chunk_rest.call(null,seq__23943__$1);
var G__23981 = c__17629__auto__;
var G__23982 = cljs.core.count.call(null,c__17629__auto__);
var G__23983 = (0);
seq__23943 = G__23980;
chunk__23944 = G__23981;
count__23945 = G__23982;
i__23946 = G__23983;
continue;
} else {
var prov = cljs.core.first.call(null,seq__23943__$1);
figwheel.client.file_reloading.path_to_name_BANG_.call(null,path,prov);

var seq__23951_23984 = cljs.core.seq.call(null,requires);
var chunk__23952_23985 = null;
var count__23953_23986 = (0);
var i__23954_23987 = (0);
while(true){
if((i__23954_23987 < count__23953_23986)){
var req_23988 = cljs.core._nth.call(null,chunk__23952_23985,i__23954_23987);
figwheel.client.file_reloading.name_to_parent_BANG_.call(null,req_23988,prov);

var G__23989 = seq__23951_23984;
var G__23990 = chunk__23952_23985;
var G__23991 = count__23953_23986;
var G__23992 = (i__23954_23987 + (1));
seq__23951_23984 = G__23989;
chunk__23952_23985 = G__23990;
count__23953_23986 = G__23991;
i__23954_23987 = G__23992;
continue;
} else {
var temp__4425__auto___23993__$1 = cljs.core.seq.call(null,seq__23951_23984);
if(temp__4425__auto___23993__$1){
var seq__23951_23994__$1 = temp__4425__auto___23993__$1;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__23951_23994__$1)){
var c__17629__auto___23995 = cljs.core.chunk_first.call(null,seq__23951_23994__$1);
var G__23996 = cljs.core.chunk_rest.call(null,seq__23951_23994__$1);
var G__23997 = c__17629__auto___23995;
var G__23998 = cljs.core.count.call(null,c__17629__auto___23995);
var G__23999 = (0);
seq__23951_23984 = G__23996;
chunk__23952_23985 = G__23997;
count__23953_23986 = G__23998;
i__23954_23987 = G__23999;
continue;
} else {
var req_24000 = cljs.core.first.call(null,seq__23951_23994__$1);
figwheel.client.file_reloading.name_to_parent_BANG_.call(null,req_24000,prov);

var G__24001 = cljs.core.next.call(null,seq__23951_23994__$1);
var G__24002 = null;
var G__24003 = (0);
var G__24004 = (0);
seq__23951_23984 = G__24001;
chunk__23952_23985 = G__24002;
count__23953_23986 = G__24003;
i__23954_23987 = G__24004;
continue;
}
} else {
}
}
break;
}

var G__24005 = cljs.core.next.call(null,seq__23943__$1);
var G__24006 = null;
var G__24007 = (0);
var G__24008 = (0);
seq__23943 = G__24005;
chunk__23944 = G__24006;
count__23945 = G__24007;
i__23946 = G__24008;
continue;
}
} else {
return null;
}
}
break;
}
});
figwheel.client.file_reloading.figwheel_require = (function figwheel$client$file_reloading$figwheel_require(src,reload){
goog.require = figwheel$client$file_reloading$figwheel_require;

if(cljs.core._EQ_.call(null,reload,"reload-all")){
var seq__24013_24017 = cljs.core.seq.call(null,figwheel.client.file_reloading.get_all_dependencies.call(null,src));
var chunk__24014_24018 = null;
var count__24015_24019 = (0);
var i__24016_24020 = (0);
while(true){
if((i__24016_24020 < count__24015_24019)){
var ns_24021 = cljs.core._nth.call(null,chunk__24014_24018,i__24016_24020);
figwheel.client.file_reloading.unprovide_BANG_.call(null,ns_24021);

var G__24022 = seq__24013_24017;
var G__24023 = chunk__24014_24018;
var G__24024 = count__24015_24019;
var G__24025 = (i__24016_24020 + (1));
seq__24013_24017 = G__24022;
chunk__24014_24018 = G__24023;
count__24015_24019 = G__24024;
i__24016_24020 = G__24025;
continue;
} else {
var temp__4425__auto___24026 = cljs.core.seq.call(null,seq__24013_24017);
if(temp__4425__auto___24026){
var seq__24013_24027__$1 = temp__4425__auto___24026;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__24013_24027__$1)){
var c__17629__auto___24028 = cljs.core.chunk_first.call(null,seq__24013_24027__$1);
var G__24029 = cljs.core.chunk_rest.call(null,seq__24013_24027__$1);
var G__24030 = c__17629__auto___24028;
var G__24031 = cljs.core.count.call(null,c__17629__auto___24028);
var G__24032 = (0);
seq__24013_24017 = G__24029;
chunk__24014_24018 = G__24030;
count__24015_24019 = G__24031;
i__24016_24020 = G__24032;
continue;
} else {
var ns_24033 = cljs.core.first.call(null,seq__24013_24027__$1);
figwheel.client.file_reloading.unprovide_BANG_.call(null,ns_24033);

var G__24034 = cljs.core.next.call(null,seq__24013_24027__$1);
var G__24035 = null;
var G__24036 = (0);
var G__24037 = (0);
seq__24013_24017 = G__24034;
chunk__24014_24018 = G__24035;
count__24015_24019 = G__24036;
i__24016_24020 = G__24037;
continue;
}
} else {
}
}
break;
}
} else {
}

if(cljs.core.truth_(reload)){
figwheel.client.file_reloading.unprovide_BANG_.call(null,src);
} else {
}

return goog.require_figwheel_backup_(src);
});
/**
 * Reusable browser REPL bootstrapping. Patches the essential functions
 *   in goog.base to support re-loading of namespaces after page load.
 */
figwheel.client.file_reloading.bootstrap_goog_base = (function figwheel$client$file_reloading$bootstrap_goog_base(){
if(cljs.core.truth_(COMPILED)){
return null;
} else {
goog.require_figwheel_backup_ = (function (){var or__16826__auto__ = goog.require__;
if(cljs.core.truth_(or__16826__auto__)){
return or__16826__auto__;
} else {
return goog.require;
}
})();

goog.isProvided_ = (function (name){
return false;
});

figwheel.client.file_reloading.setup_path__GT_name_BANG_.call(null);

figwheel.client.file_reloading.setup_ns__GT_dependents_BANG_.call(null);

goog.addDependency_figwheel_backup_ = goog.addDependency;

goog.addDependency = (function() { 
var G__24038__delegate = function (args){
cljs.core.apply.call(null,figwheel.client.file_reloading.addDependency,args);

return cljs.core.apply.call(null,goog.addDependency_figwheel_backup_,args);
};
var G__24038 = function (var_args){
var args = null;
if (arguments.length > 0) {
var G__24039__i = 0, G__24039__a = new Array(arguments.length -  0);
while (G__24039__i < G__24039__a.length) {G__24039__a[G__24039__i] = arguments[G__24039__i + 0]; ++G__24039__i;}
  args = new cljs.core.IndexedSeq(G__24039__a,0);
} 
return G__24038__delegate.call(this,args);};
G__24038.cljs$lang$maxFixedArity = 0;
G__24038.cljs$lang$applyTo = (function (arglist__24040){
var args = cljs.core.seq(arglist__24040);
return G__24038__delegate(args);
});
G__24038.cljs$core$IFn$_invoke$arity$variadic = G__24038__delegate;
return G__24038;
})()
;

goog.constructNamespace_("cljs.user");

goog.global.CLOSURE_IMPORT_SCRIPT = figwheel.client.file_reloading.queued_file_reload;

return goog.require = figwheel.client.file_reloading.figwheel_require;
}
});
figwheel.client.file_reloading.patch_goog_base = (function figwheel$client$file_reloading$patch_goog_base(){
if(typeof figwheel.client.file_reloading.bootstrapped_cljs !== 'undefined'){
return null;
} else {
figwheel.client.file_reloading.bootstrapped_cljs = (function (){
figwheel.client.file_reloading.bootstrap_goog_base.call(null);

return true;
})()
;
}
});
figwheel.client.file_reloading.reload_file_STAR_ = (function (){var pred__24042 = cljs.core._EQ_;
var expr__24043 = figwheel.client.utils.host_env_QMARK_.call(null);
if(cljs.core.truth_(pred__24042.call(null,new cljs.core.Keyword(null,"node","node",581201198),expr__24043))){
var path_parts = ((function (pred__24042,expr__24043){
return (function (p1__24041_SHARP_){
return clojure.string.split.call(null,p1__24041_SHARP_,/[\/\\]/);
});})(pred__24042,expr__24043))
;
var sep = (cljs.core.truth_(cljs.core.re_matches.call(null,/win.*/,process.platform))?"\\":"/");
var root = clojure.string.join.call(null,sep,cljs.core.pop.call(null,cljs.core.pop.call(null,path_parts.call(null,__dirname))));
return ((function (path_parts,sep,root,pred__24042,expr__24043){
return (function (request_url,callback){

var cache_path = clojure.string.join.call(null,sep,cljs.core.cons.call(null,root,path_parts.call(null,figwheel.client.file_reloading.fix_node_request_url.call(null,request_url))));
(require.cache[cache_path] = null);

return callback.call(null,(function (){try{return require(cache_path);
}catch (e24045){if((e24045 instanceof Error)){
var e = e24045;
figwheel.client.utils.log.call(null,new cljs.core.Keyword(null,"error","error",-978969032),[cljs.core.str("Figwheel: Error loading file "),cljs.core.str(cache_path)].join(''));

figwheel.client.utils.log.call(null,new cljs.core.Keyword(null,"error","error",-978969032),e.stack);

return false;
} else {
throw e24045;

}
}})());
});
;})(path_parts,sep,root,pred__24042,expr__24043))
} else {
if(cljs.core.truth_(pred__24042.call(null,new cljs.core.Keyword(null,"html","html",-998796897),expr__24043))){
return ((function (pred__24042,expr__24043){
return (function (request_url,callback){

var deferred = goog.net.jsloader.load(figwheel.client.file_reloading.add_cache_buster.call(null,request_url),{"cleanupWhenDone": true});
deferred.addCallback(((function (deferred,pred__24042,expr__24043){
return (function (){
return cljs.core.apply.call(null,callback,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [true], null));
});})(deferred,pred__24042,expr__24043))
);

return deferred.addErrback(((function (deferred,pred__24042,expr__24043){
return (function (){
return cljs.core.apply.call(null,callback,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [false], null));
});})(deferred,pred__24042,expr__24043))
);
});
;})(pred__24042,expr__24043))
} else {
return ((function (pred__24042,expr__24043){
return (function (a,b){
throw "Reload not defined for this platform";
});
;})(pred__24042,expr__24043))
}
}
})();
figwheel.client.file_reloading.reload_file = (function figwheel$client$file_reloading$reload_file(p__24046,callback){
var map__24049 = p__24046;
var map__24049__$1 = ((((!((map__24049 == null)))?((((map__24049.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24049.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24049):map__24049);
var file_msg = map__24049__$1;
var request_url = cljs.core.get.call(null,map__24049__$1,new cljs.core.Keyword(null,"request-url","request-url",2100346596));

figwheel.client.utils.debug_prn.call(null,[cljs.core.str("FigWheel: Attempting to load "),cljs.core.str(request_url)].join(''));

return figwheel.client.file_reloading.reload_file_STAR_.call(null,request_url,((function (map__24049,map__24049__$1,file_msg,request_url){
return (function (success_QMARK_){
if(cljs.core.truth_(success_QMARK_)){
figwheel.client.utils.debug_prn.call(null,[cljs.core.str("FigWheel: Successfully loaded "),cljs.core.str(request_url)].join(''));

return cljs.core.apply.call(null,callback,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.assoc.call(null,file_msg,new cljs.core.Keyword(null,"loaded-file","loaded-file",-168399375),true)], null));
} else {
figwheel.client.utils.log.call(null,new cljs.core.Keyword(null,"error","error",-978969032),[cljs.core.str("Figwheel: Error loading file "),cljs.core.str(request_url)].join(''));

return cljs.core.apply.call(null,callback,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [file_msg], null));
}
});})(map__24049,map__24049__$1,file_msg,request_url))
);
});
if(typeof figwheel.client.file_reloading.reload_chan !== 'undefined'){
} else {
figwheel.client.file_reloading.reload_chan = cljs.core.async.chan.call(null);
}
if(typeof figwheel.client.file_reloading.on_load_callbacks !== 'undefined'){
} else {
figwheel.client.file_reloading.on_load_callbacks = cljs.core.atom.call(null,cljs.core.PersistentArrayMap.EMPTY);
}
if(typeof figwheel.client.file_reloading.dependencies_loaded !== 'undefined'){
} else {
figwheel.client.file_reloading.dependencies_loaded = cljs.core.atom.call(null,cljs.core.PersistentVector.EMPTY);
}
figwheel.client.file_reloading.blocking_load = (function figwheel$client$file_reloading$blocking_load(url){
var out = cljs.core.async.chan.call(null);
figwheel.client.file_reloading.reload_file.call(null,new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"request-url","request-url",2100346596),url], null),((function (out){
return (function (file_msg){
cljs.core.async.put_BANG_.call(null,out,file_msg);

return cljs.core.async.close_BANG_.call(null,out);
});})(out))
);

return out;
});
if(typeof figwheel.client.file_reloading.reloader_loop !== 'undefined'){
} else {
figwheel.client.file_reloading.reloader_loop = (function (){var c__20950__auto__ = cljs.core.async.chan.call(null,(1));
cljs.core.async.impl.dispatch.run.call(null,((function (c__20950__auto__){
return (function (){
var f__20951__auto__ = (function (){var switch__20838__auto__ = ((function (c__20950__auto__){
return (function (state_24073){
var state_val_24074 = (state_24073[(1)]);
if((state_val_24074 === (7))){
var inst_24069 = (state_24073[(2)]);
var state_24073__$1 = state_24073;
var statearr_24075_24095 = state_24073__$1;
(statearr_24075_24095[(2)] = inst_24069);

(statearr_24075_24095[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24074 === (1))){
var state_24073__$1 = state_24073;
var statearr_24076_24096 = state_24073__$1;
(statearr_24076_24096[(2)] = null);

(statearr_24076_24096[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24074 === (4))){
var inst_24053 = (state_24073[(7)]);
var inst_24053__$1 = (state_24073[(2)]);
var state_24073__$1 = (function (){var statearr_24077 = state_24073;
(statearr_24077[(7)] = inst_24053__$1);

return statearr_24077;
})();
if(cljs.core.truth_(inst_24053__$1)){
var statearr_24078_24097 = state_24073__$1;
(statearr_24078_24097[(1)] = (5));

} else {
var statearr_24079_24098 = state_24073__$1;
(statearr_24079_24098[(1)] = (6));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24074 === (6))){
var state_24073__$1 = state_24073;
var statearr_24080_24099 = state_24073__$1;
(statearr_24080_24099[(2)] = null);

(statearr_24080_24099[(1)] = (7));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24074 === (3))){
var inst_24071 = (state_24073[(2)]);
var state_24073__$1 = state_24073;
return cljs.core.async.impl.ioc_helpers.return_chan.call(null,state_24073__$1,inst_24071);
} else {
if((state_val_24074 === (2))){
var state_24073__$1 = state_24073;
return cljs.core.async.impl.ioc_helpers.take_BANG_.call(null,state_24073__$1,(4),figwheel.client.file_reloading.reload_chan);
} else {
if((state_val_24074 === (11))){
var inst_24065 = (state_24073[(2)]);
var state_24073__$1 = (function (){var statearr_24081 = state_24073;
(statearr_24081[(8)] = inst_24065);

return statearr_24081;
})();
var statearr_24082_24100 = state_24073__$1;
(statearr_24082_24100[(2)] = null);

(statearr_24082_24100[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24074 === (9))){
var inst_24057 = (state_24073[(9)]);
var inst_24059 = (state_24073[(10)]);
var inst_24061 = inst_24059.call(null,inst_24057);
var state_24073__$1 = state_24073;
var statearr_24083_24101 = state_24073__$1;
(statearr_24083_24101[(2)] = inst_24061);

(statearr_24083_24101[(1)] = (11));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24074 === (5))){
var inst_24053 = (state_24073[(7)]);
var inst_24055 = figwheel.client.file_reloading.blocking_load.call(null,inst_24053);
var state_24073__$1 = state_24073;
return cljs.core.async.impl.ioc_helpers.take_BANG_.call(null,state_24073__$1,(8),inst_24055);
} else {
if((state_val_24074 === (10))){
var inst_24057 = (state_24073[(9)]);
var inst_24063 = cljs.core.swap_BANG_.call(null,figwheel.client.file_reloading.dependencies_loaded,cljs.core.conj,inst_24057);
var state_24073__$1 = state_24073;
var statearr_24084_24102 = state_24073__$1;
(statearr_24084_24102[(2)] = inst_24063);

(statearr_24084_24102[(1)] = (11));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24074 === (8))){
var inst_24059 = (state_24073[(10)]);
var inst_24053 = (state_24073[(7)]);
var inst_24057 = (state_24073[(2)]);
var inst_24058 = cljs.core.deref.call(null,figwheel.client.file_reloading.on_load_callbacks);
var inst_24059__$1 = cljs.core.get.call(null,inst_24058,inst_24053);
var state_24073__$1 = (function (){var statearr_24085 = state_24073;
(statearr_24085[(9)] = inst_24057);

(statearr_24085[(10)] = inst_24059__$1);

return statearr_24085;
})();
if(cljs.core.truth_(inst_24059__$1)){
var statearr_24086_24103 = state_24073__$1;
(statearr_24086_24103[(1)] = (9));

} else {
var statearr_24087_24104 = state_24073__$1;
(statearr_24087_24104[(1)] = (10));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
});})(c__20950__auto__))
;
return ((function (switch__20838__auto__,c__20950__auto__){
return (function() {
var figwheel$client$file_reloading$state_machine__20839__auto__ = null;
var figwheel$client$file_reloading$state_machine__20839__auto____0 = (function (){
var statearr_24091 = [null,null,null,null,null,null,null,null,null,null,null];
(statearr_24091[(0)] = figwheel$client$file_reloading$state_machine__20839__auto__);

(statearr_24091[(1)] = (1));

return statearr_24091;
});
var figwheel$client$file_reloading$state_machine__20839__auto____1 = (function (state_24073){
while(true){
var ret_value__20840__auto__ = (function (){try{while(true){
var result__20841__auto__ = switch__20838__auto__.call(null,state_24073);
if(cljs.core.keyword_identical_QMARK_.call(null,result__20841__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__20841__auto__;
}
break;
}
}catch (e24092){if((e24092 instanceof Object)){
var ex__20842__auto__ = e24092;
var statearr_24093_24105 = state_24073;
(statearr_24093_24105[(5)] = ex__20842__auto__);


cljs.core.async.impl.ioc_helpers.process_exception.call(null,state_24073);

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
throw e24092;

}
}})();
if(cljs.core.keyword_identical_QMARK_.call(null,ret_value__20840__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__24106 = state_24073;
state_24073 = G__24106;
continue;
} else {
return ret_value__20840__auto__;
}
break;
}
});
figwheel$client$file_reloading$state_machine__20839__auto__ = function(state_24073){
switch(arguments.length){
case 0:
return figwheel$client$file_reloading$state_machine__20839__auto____0.call(this);
case 1:
return figwheel$client$file_reloading$state_machine__20839__auto____1.call(this,state_24073);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
figwheel$client$file_reloading$state_machine__20839__auto__.cljs$core$IFn$_invoke$arity$0 = figwheel$client$file_reloading$state_machine__20839__auto____0;
figwheel$client$file_reloading$state_machine__20839__auto__.cljs$core$IFn$_invoke$arity$1 = figwheel$client$file_reloading$state_machine__20839__auto____1;
return figwheel$client$file_reloading$state_machine__20839__auto__;
})()
;})(switch__20838__auto__,c__20950__auto__))
})();
var state__20952__auto__ = (function (){var statearr_24094 = f__20951__auto__.call(null);
(statearr_24094[cljs.core.async.impl.ioc_helpers.USER_START_IDX] = c__20950__auto__);

return statearr_24094;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped.call(null,state__20952__auto__);
});})(c__20950__auto__))
);

return c__20950__auto__;
})();
}
figwheel.client.file_reloading.queued_file_reload = (function figwheel$client$file_reloading$queued_file_reload(url){
return cljs.core.async.put_BANG_.call(null,figwheel.client.file_reloading.reload_chan,url);
});
figwheel.client.file_reloading.require_with_callback = (function figwheel$client$file_reloading$require_with_callback(p__24107,callback){
var map__24110 = p__24107;
var map__24110__$1 = ((((!((map__24110 == null)))?((((map__24110.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24110.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24110):map__24110);
var file_msg = map__24110__$1;
var namespace = cljs.core.get.call(null,map__24110__$1,new cljs.core.Keyword(null,"namespace","namespace",-377510372));
var request_url = figwheel.client.file_reloading.resolve_ns.call(null,namespace);
cljs.core.swap_BANG_.call(null,figwheel.client.file_reloading.on_load_callbacks,cljs.core.assoc,request_url,((function (request_url,map__24110,map__24110__$1,file_msg,namespace){
return (function (file_msg_SINGLEQUOTE_){
cljs.core.swap_BANG_.call(null,figwheel.client.file_reloading.on_load_callbacks,cljs.core.dissoc,request_url);

return cljs.core.apply.call(null,callback,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.merge.call(null,file_msg,cljs.core.select_keys.call(null,file_msg_SINGLEQUOTE_,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"loaded-file","loaded-file",-168399375)], null)))], null));
});})(request_url,map__24110,map__24110__$1,file_msg,namespace))
);

return figwheel.client.file_reloading.figwheel_require.call(null,cljs.core.name.call(null,namespace),true);
});
figwheel.client.file_reloading.reload_file_QMARK_ = (function figwheel$client$file_reloading$reload_file_QMARK_(p__24112){
var map__24115 = p__24112;
var map__24115__$1 = ((((!((map__24115 == null)))?((((map__24115.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24115.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24115):map__24115);
var file_msg = map__24115__$1;
var namespace = cljs.core.get.call(null,map__24115__$1,new cljs.core.Keyword(null,"namespace","namespace",-377510372));

var meta_pragmas = cljs.core.get.call(null,cljs.core.deref.call(null,figwheel.client.file_reloading.figwheel_meta_pragmas),cljs.core.name.call(null,namespace));
var and__16814__auto__ = cljs.core.not.call(null,new cljs.core.Keyword(null,"figwheel-no-load","figwheel-no-load",-555840179).cljs$core$IFn$_invoke$arity$1(meta_pragmas));
if(and__16814__auto__){
var or__16826__auto__ = new cljs.core.Keyword(null,"figwheel-always","figwheel-always",799819691).cljs$core$IFn$_invoke$arity$1(meta_pragmas);
if(cljs.core.truth_(or__16826__auto__)){
return or__16826__auto__;
} else {
var or__16826__auto____$1 = new cljs.core.Keyword(null,"figwheel-load","figwheel-load",1316089175).cljs$core$IFn$_invoke$arity$1(meta_pragmas);
if(cljs.core.truth_(or__16826__auto____$1)){
return or__16826__auto____$1;
} else {
return figwheel.client.file_reloading.provided_QMARK_.call(null,cljs.core.name.call(null,namespace));
}
}
} else {
return and__16814__auto__;
}
});
figwheel.client.file_reloading.js_reload = (function figwheel$client$file_reloading$js_reload(p__24117,callback){
var map__24120 = p__24117;
var map__24120__$1 = ((((!((map__24120 == null)))?((((map__24120.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24120.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24120):map__24120);
var file_msg = map__24120__$1;
var request_url = cljs.core.get.call(null,map__24120__$1,new cljs.core.Keyword(null,"request-url","request-url",2100346596));
var namespace = cljs.core.get.call(null,map__24120__$1,new cljs.core.Keyword(null,"namespace","namespace",-377510372));

if(cljs.core.truth_(figwheel.client.file_reloading.reload_file_QMARK_.call(null,file_msg))){
return figwheel.client.file_reloading.require_with_callback.call(null,file_msg,callback);
} else {
figwheel.client.utils.debug_prn.call(null,[cljs.core.str("Figwheel: Not trying to load file "),cljs.core.str(request_url)].join(''));

return cljs.core.apply.call(null,callback,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [file_msg], null));
}
});
figwheel.client.file_reloading.reload_js_file = (function figwheel$client$file_reloading$reload_js_file(file_msg){
var out = cljs.core.async.chan.call(null);
figwheel.client.file_reloading.js_reload.call(null,file_msg,((function (out){
return (function (url){
cljs.core.async.put_BANG_.call(null,out,url);

return cljs.core.async.close_BANG_.call(null,out);
});})(out))
);

return out;
});
/**
 * Returns a chanel with one collection of loaded filenames on it.
 */
figwheel.client.file_reloading.load_all_js_files = (function figwheel$client$file_reloading$load_all_js_files(files){
var out = cljs.core.async.chan.call(null);
var c__20950__auto___24208 = cljs.core.async.chan.call(null,(1));
cljs.core.async.impl.dispatch.run.call(null,((function (c__20950__auto___24208,out){
return (function (){
var f__20951__auto__ = (function (){var switch__20838__auto__ = ((function (c__20950__auto___24208,out){
return (function (state_24190){
var state_val_24191 = (state_24190[(1)]);
if((state_val_24191 === (1))){
var inst_24168 = cljs.core.nth.call(null,files,(0),null);
var inst_24169 = cljs.core.nthnext.call(null,files,(1));
var inst_24170 = files;
var state_24190__$1 = (function (){var statearr_24192 = state_24190;
(statearr_24192[(7)] = inst_24168);

(statearr_24192[(8)] = inst_24169);

(statearr_24192[(9)] = inst_24170);

return statearr_24192;
})();
var statearr_24193_24209 = state_24190__$1;
(statearr_24193_24209[(2)] = null);

(statearr_24193_24209[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24191 === (2))){
var inst_24173 = (state_24190[(10)]);
var inst_24170 = (state_24190[(9)]);
var inst_24173__$1 = cljs.core.nth.call(null,inst_24170,(0),null);
var inst_24174 = cljs.core.nthnext.call(null,inst_24170,(1));
var inst_24175 = (inst_24173__$1 == null);
var inst_24176 = cljs.core.not.call(null,inst_24175);
var state_24190__$1 = (function (){var statearr_24194 = state_24190;
(statearr_24194[(11)] = inst_24174);

(statearr_24194[(10)] = inst_24173__$1);

return statearr_24194;
})();
if(inst_24176){
var statearr_24195_24210 = state_24190__$1;
(statearr_24195_24210[(1)] = (4));

} else {
var statearr_24196_24211 = state_24190__$1;
(statearr_24196_24211[(1)] = (5));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24191 === (3))){
var inst_24188 = (state_24190[(2)]);
var state_24190__$1 = state_24190;
return cljs.core.async.impl.ioc_helpers.return_chan.call(null,state_24190__$1,inst_24188);
} else {
if((state_val_24191 === (4))){
var inst_24173 = (state_24190[(10)]);
var inst_24178 = figwheel.client.file_reloading.reload_js_file.call(null,inst_24173);
var state_24190__$1 = state_24190;
return cljs.core.async.impl.ioc_helpers.take_BANG_.call(null,state_24190__$1,(7),inst_24178);
} else {
if((state_val_24191 === (5))){
var inst_24184 = cljs.core.async.close_BANG_.call(null,out);
var state_24190__$1 = state_24190;
var statearr_24197_24212 = state_24190__$1;
(statearr_24197_24212[(2)] = inst_24184);

(statearr_24197_24212[(1)] = (6));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24191 === (6))){
var inst_24186 = (state_24190[(2)]);
var state_24190__$1 = state_24190;
var statearr_24198_24213 = state_24190__$1;
(statearr_24198_24213[(2)] = inst_24186);

(statearr_24198_24213[(1)] = (3));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24191 === (7))){
var inst_24174 = (state_24190[(11)]);
var inst_24180 = (state_24190[(2)]);
var inst_24181 = cljs.core.async.put_BANG_.call(null,out,inst_24180);
var inst_24170 = inst_24174;
var state_24190__$1 = (function (){var statearr_24199 = state_24190;
(statearr_24199[(9)] = inst_24170);

(statearr_24199[(12)] = inst_24181);

return statearr_24199;
})();
var statearr_24200_24214 = state_24190__$1;
(statearr_24200_24214[(2)] = null);

(statearr_24200_24214[(1)] = (2));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
});})(c__20950__auto___24208,out))
;
return ((function (switch__20838__auto__,c__20950__auto___24208,out){
return (function() {
var figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto__ = null;
var figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto____0 = (function (){
var statearr_24204 = [null,null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_24204[(0)] = figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto__);

(statearr_24204[(1)] = (1));

return statearr_24204;
});
var figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto____1 = (function (state_24190){
while(true){
var ret_value__20840__auto__ = (function (){try{while(true){
var result__20841__auto__ = switch__20838__auto__.call(null,state_24190);
if(cljs.core.keyword_identical_QMARK_.call(null,result__20841__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__20841__auto__;
}
break;
}
}catch (e24205){if((e24205 instanceof Object)){
var ex__20842__auto__ = e24205;
var statearr_24206_24215 = state_24190;
(statearr_24206_24215[(5)] = ex__20842__auto__);


cljs.core.async.impl.ioc_helpers.process_exception.call(null,state_24190);

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
throw e24205;

}
}})();
if(cljs.core.keyword_identical_QMARK_.call(null,ret_value__20840__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__24216 = state_24190;
state_24190 = G__24216;
continue;
} else {
return ret_value__20840__auto__;
}
break;
}
});
figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto__ = function(state_24190){
switch(arguments.length){
case 0:
return figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto____0.call(this);
case 1:
return figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto____1.call(this,state_24190);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto__.cljs$core$IFn$_invoke$arity$0 = figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto____0;
figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto__.cljs$core$IFn$_invoke$arity$1 = figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto____1;
return figwheel$client$file_reloading$load_all_js_files_$_state_machine__20839__auto__;
})()
;})(switch__20838__auto__,c__20950__auto___24208,out))
})();
var state__20952__auto__ = (function (){var statearr_24207 = f__20951__auto__.call(null);
(statearr_24207[cljs.core.async.impl.ioc_helpers.USER_START_IDX] = c__20950__auto___24208);

return statearr_24207;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped.call(null,state__20952__auto__);
});})(c__20950__auto___24208,out))
);


return cljs.core.async.into.call(null,cljs.core.PersistentVector.EMPTY,out);
});
figwheel.client.file_reloading.eval_body = (function figwheel$client$file_reloading$eval_body(p__24217,opts){
var map__24221 = p__24217;
var map__24221__$1 = ((((!((map__24221 == null)))?((((map__24221.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24221.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24221):map__24221);
var eval_body__$1 = cljs.core.get.call(null,map__24221__$1,new cljs.core.Keyword(null,"eval-body","eval-body",-907279883));
var file = cljs.core.get.call(null,map__24221__$1,new cljs.core.Keyword(null,"file","file",-1269645878));
if(cljs.core.truth_((function (){var and__16814__auto__ = eval_body__$1;
if(cljs.core.truth_(and__16814__auto__)){
return typeof eval_body__$1 === 'string';
} else {
return and__16814__auto__;
}
})())){
var code = eval_body__$1;
try{figwheel.client.utils.debug_prn.call(null,[cljs.core.str("Evaling file "),cljs.core.str(file)].join(''));

return figwheel.client.utils.eval_helper.call(null,code,opts);
}catch (e24223){var e = e24223;
return figwheel.client.utils.log.call(null,new cljs.core.Keyword(null,"error","error",-978969032),[cljs.core.str("Unable to evaluate "),cljs.core.str(file)].join(''));
}} else {
return null;
}
});
figwheel.client.file_reloading.expand_files = (function figwheel$client$file_reloading$expand_files(files){
var deps = figwheel.client.file_reloading.get_all_dependents.call(null,cljs.core.map.call(null,new cljs.core.Keyword(null,"namespace","namespace",-377510372),files));
return cljs.core.filter.call(null,cljs.core.comp.call(null,cljs.core.not,new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 1, ["figwheel.connect",null], null), null),new cljs.core.Keyword(null,"namespace","namespace",-377510372)),cljs.core.map.call(null,((function (deps){
return (function (n){
var temp__4423__auto__ = cljs.core.first.call(null,cljs.core.filter.call(null,((function (deps){
return (function (p1__24224_SHARP_){
return cljs.core._EQ_.call(null,new cljs.core.Keyword(null,"namespace","namespace",-377510372).cljs$core$IFn$_invoke$arity$1(p1__24224_SHARP_),n);
});})(deps))
,files));
if(cljs.core.truth_(temp__4423__auto__)){
var file_msg = temp__4423__auto__;
return file_msg;
} else {
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"namespace","namespace",-377510372),new cljs.core.Keyword(null,"namespace","namespace",-377510372),n], null);
}
});})(deps))
,deps));
});
figwheel.client.file_reloading.sort_files = (function figwheel$client$file_reloading$sort_files(files){
if((cljs.core.count.call(null,files) <= (1))){
return files;
} else {
var keep_files = cljs.core.set.call(null,cljs.core.keep.call(null,new cljs.core.Keyword(null,"namespace","namespace",-377510372),files));
return cljs.core.filter.call(null,cljs.core.comp.call(null,keep_files,new cljs.core.Keyword(null,"namespace","namespace",-377510372)),figwheel.client.file_reloading.expand_files.call(null,files));
}
});
figwheel.client.file_reloading.get_figwheel_always = (function figwheel$client$file_reloading$get_figwheel_always(){
return cljs.core.map.call(null,(function (p__24229){
var vec__24230 = p__24229;
var k = cljs.core.nth.call(null,vec__24230,(0),null);
var v = cljs.core.nth.call(null,vec__24230,(1),null);
return new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"namespace","namespace",-377510372),k,new cljs.core.Keyword(null,"type","type",1174270348),new cljs.core.Keyword(null,"namespace","namespace",-377510372)], null);
}),cljs.core.filter.call(null,(function (p__24231){
var vec__24232 = p__24231;
var k = cljs.core.nth.call(null,vec__24232,(0),null);
var v = cljs.core.nth.call(null,vec__24232,(1),null);
return new cljs.core.Keyword(null,"figwheel-always","figwheel-always",799819691).cljs$core$IFn$_invoke$arity$1(v);
}),cljs.core.deref.call(null,figwheel.client.file_reloading.figwheel_meta_pragmas)));
});
figwheel.client.file_reloading.reload_js_files = (function figwheel$client$file_reloading$reload_js_files(p__24236,p__24237){
var map__24484 = p__24236;
var map__24484__$1 = ((((!((map__24484 == null)))?((((map__24484.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24484.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24484):map__24484);
var opts = map__24484__$1;
var before_jsload = cljs.core.get.call(null,map__24484__$1,new cljs.core.Keyword(null,"before-jsload","before-jsload",-847513128));
var on_jsload = cljs.core.get.call(null,map__24484__$1,new cljs.core.Keyword(null,"on-jsload","on-jsload",-395756602));
var reload_dependents = cljs.core.get.call(null,map__24484__$1,new cljs.core.Keyword(null,"reload-dependents","reload-dependents",-956865430));
var map__24485 = p__24237;
var map__24485__$1 = ((((!((map__24485 == null)))?((((map__24485.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24485.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24485):map__24485);
var msg = map__24485__$1;
var files = cljs.core.get.call(null,map__24485__$1,new cljs.core.Keyword(null,"files","files",-472457450));
var figwheel_meta = cljs.core.get.call(null,map__24485__$1,new cljs.core.Keyword(null,"figwheel-meta","figwheel-meta",-225970237));
var recompile_dependents = cljs.core.get.call(null,map__24485__$1,new cljs.core.Keyword(null,"recompile-dependents","recompile-dependents",523804171));
if(cljs.core.empty_QMARK_.call(null,figwheel_meta)){
} else {
cljs.core.reset_BANG_.call(null,figwheel.client.file_reloading.figwheel_meta_pragmas,figwheel_meta);
}

var c__20950__auto__ = cljs.core.async.chan.call(null,(1));
cljs.core.async.impl.dispatch.run.call(null,((function (c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents){
return (function (){
var f__20951__auto__ = (function (){var switch__20838__auto__ = ((function (c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents){
return (function (state_24638){
var state_val_24639 = (state_24638[(1)]);
if((state_val_24639 === (7))){
var inst_24501 = (state_24638[(7)]);
var inst_24500 = (state_24638[(8)]);
var inst_24502 = (state_24638[(9)]);
var inst_24499 = (state_24638[(10)]);
var inst_24507 = cljs.core._nth.call(null,inst_24500,inst_24502);
var inst_24508 = figwheel.client.file_reloading.eval_body.call(null,inst_24507,opts);
var inst_24509 = (inst_24502 + (1));
var tmp24640 = inst_24501;
var tmp24641 = inst_24500;
var tmp24642 = inst_24499;
var inst_24499__$1 = tmp24642;
var inst_24500__$1 = tmp24641;
var inst_24501__$1 = tmp24640;
var inst_24502__$1 = inst_24509;
var state_24638__$1 = (function (){var statearr_24643 = state_24638;
(statearr_24643[(7)] = inst_24501__$1);

(statearr_24643[(8)] = inst_24500__$1);

(statearr_24643[(9)] = inst_24502__$1);

(statearr_24643[(11)] = inst_24508);

(statearr_24643[(10)] = inst_24499__$1);

return statearr_24643;
})();
var statearr_24644_24730 = state_24638__$1;
(statearr_24644_24730[(2)] = null);

(statearr_24644_24730[(1)] = (5));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (20))){
var inst_24542 = (state_24638[(12)]);
var inst_24550 = figwheel.client.file_reloading.sort_files.call(null,inst_24542);
var state_24638__$1 = state_24638;
var statearr_24645_24731 = state_24638__$1;
(statearr_24645_24731[(2)] = inst_24550);

(statearr_24645_24731[(1)] = (21));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (27))){
var state_24638__$1 = state_24638;
var statearr_24646_24732 = state_24638__$1;
(statearr_24646_24732[(2)] = null);

(statearr_24646_24732[(1)] = (28));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (1))){
var inst_24491 = (state_24638[(13)]);
var inst_24488 = before_jsload.call(null,files);
var inst_24489 = figwheel.client.file_reloading.before_jsload_custom_event.call(null,files);
var inst_24490 = (function (){return ((function (inst_24491,inst_24488,inst_24489,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents){
return (function (p1__24233_SHARP_){
return new cljs.core.Keyword(null,"eval-body","eval-body",-907279883).cljs$core$IFn$_invoke$arity$1(p1__24233_SHARP_);
});
;})(inst_24491,inst_24488,inst_24489,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents))
})();
var inst_24491__$1 = cljs.core.filter.call(null,inst_24490,files);
var inst_24492 = cljs.core.not_empty.call(null,inst_24491__$1);
var state_24638__$1 = (function (){var statearr_24647 = state_24638;
(statearr_24647[(13)] = inst_24491__$1);

(statearr_24647[(14)] = inst_24489);

(statearr_24647[(15)] = inst_24488);

return statearr_24647;
})();
if(cljs.core.truth_(inst_24492)){
var statearr_24648_24733 = state_24638__$1;
(statearr_24648_24733[(1)] = (2));

} else {
var statearr_24649_24734 = state_24638__$1;
(statearr_24649_24734[(1)] = (3));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (24))){
var state_24638__$1 = state_24638;
var statearr_24650_24735 = state_24638__$1;
(statearr_24650_24735[(2)] = null);

(statearr_24650_24735[(1)] = (25));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (39))){
var inst_24592 = (state_24638[(16)]);
var state_24638__$1 = state_24638;
var statearr_24651_24736 = state_24638__$1;
(statearr_24651_24736[(2)] = inst_24592);

(statearr_24651_24736[(1)] = (40));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (46))){
var inst_24633 = (state_24638[(2)]);
var state_24638__$1 = state_24638;
var statearr_24652_24737 = state_24638__$1;
(statearr_24652_24737[(2)] = inst_24633);

(statearr_24652_24737[(1)] = (31));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (4))){
var inst_24536 = (state_24638[(2)]);
var inst_24537 = cljs.core.List.EMPTY;
var inst_24538 = cljs.core.reset_BANG_.call(null,figwheel.client.file_reloading.dependencies_loaded,inst_24537);
var inst_24539 = (function (){return ((function (inst_24536,inst_24537,inst_24538,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents){
return (function (p1__24234_SHARP_){
var and__16814__auto__ = new cljs.core.Keyword(null,"namespace","namespace",-377510372).cljs$core$IFn$_invoke$arity$1(p1__24234_SHARP_);
if(cljs.core.truth_(and__16814__auto__)){
return cljs.core.not.call(null,new cljs.core.Keyword(null,"eval-body","eval-body",-907279883).cljs$core$IFn$_invoke$arity$1(p1__24234_SHARP_));
} else {
return and__16814__auto__;
}
});
;})(inst_24536,inst_24537,inst_24538,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents))
})();
var inst_24540 = cljs.core.filter.call(null,inst_24539,files);
var inst_24541 = figwheel.client.file_reloading.get_figwheel_always.call(null);
var inst_24542 = cljs.core.concat.call(null,inst_24540,inst_24541);
var state_24638__$1 = (function (){var statearr_24653 = state_24638;
(statearr_24653[(17)] = inst_24536);

(statearr_24653[(18)] = inst_24538);

(statearr_24653[(12)] = inst_24542);

return statearr_24653;
})();
if(cljs.core.truth_(reload_dependents)){
var statearr_24654_24738 = state_24638__$1;
(statearr_24654_24738[(1)] = (16));

} else {
var statearr_24655_24739 = state_24638__$1;
(statearr_24655_24739[(1)] = (17));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (15))){
var inst_24526 = (state_24638[(2)]);
var state_24638__$1 = state_24638;
var statearr_24656_24740 = state_24638__$1;
(statearr_24656_24740[(2)] = inst_24526);

(statearr_24656_24740[(1)] = (12));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (21))){
var inst_24552 = (state_24638[(19)]);
var inst_24552__$1 = (state_24638[(2)]);
var inst_24553 = figwheel.client.file_reloading.load_all_js_files.call(null,inst_24552__$1);
var state_24638__$1 = (function (){var statearr_24657 = state_24638;
(statearr_24657[(19)] = inst_24552__$1);

return statearr_24657;
})();
return cljs.core.async.impl.ioc_helpers.take_BANG_.call(null,state_24638__$1,(22),inst_24553);
} else {
if((state_val_24639 === (31))){
var inst_24636 = (state_24638[(2)]);
var state_24638__$1 = state_24638;
return cljs.core.async.impl.ioc_helpers.return_chan.call(null,state_24638__$1,inst_24636);
} else {
if((state_val_24639 === (32))){
var inst_24592 = (state_24638[(16)]);
var inst_24597 = inst_24592.cljs$lang$protocol_mask$partition0$;
var inst_24598 = (inst_24597 & (64));
var inst_24599 = inst_24592.cljs$core$ISeq$;
var inst_24600 = (inst_24598) || (inst_24599);
var state_24638__$1 = state_24638;
if(cljs.core.truth_(inst_24600)){
var statearr_24658_24741 = state_24638__$1;
(statearr_24658_24741[(1)] = (35));

} else {
var statearr_24659_24742 = state_24638__$1;
(statearr_24659_24742[(1)] = (36));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (40))){
var inst_24613 = (state_24638[(20)]);
var inst_24612 = (state_24638[(2)]);
var inst_24613__$1 = cljs.core.get.call(null,inst_24612,new cljs.core.Keyword(null,"figwheel-no-load","figwheel-no-load",-555840179));
var inst_24614 = cljs.core.get.call(null,inst_24612,new cljs.core.Keyword(null,"not-required","not-required",-950359114));
var inst_24615 = cljs.core.not_empty.call(null,inst_24613__$1);
var state_24638__$1 = (function (){var statearr_24660 = state_24638;
(statearr_24660[(20)] = inst_24613__$1);

(statearr_24660[(21)] = inst_24614);

return statearr_24660;
})();
if(cljs.core.truth_(inst_24615)){
var statearr_24661_24743 = state_24638__$1;
(statearr_24661_24743[(1)] = (41));

} else {
var statearr_24662_24744 = state_24638__$1;
(statearr_24662_24744[(1)] = (42));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (33))){
var state_24638__$1 = state_24638;
var statearr_24663_24745 = state_24638__$1;
(statearr_24663_24745[(2)] = false);

(statearr_24663_24745[(1)] = (34));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (13))){
var inst_24512 = (state_24638[(22)]);
var inst_24516 = cljs.core.chunk_first.call(null,inst_24512);
var inst_24517 = cljs.core.chunk_rest.call(null,inst_24512);
var inst_24518 = cljs.core.count.call(null,inst_24516);
var inst_24499 = inst_24517;
var inst_24500 = inst_24516;
var inst_24501 = inst_24518;
var inst_24502 = (0);
var state_24638__$1 = (function (){var statearr_24664 = state_24638;
(statearr_24664[(7)] = inst_24501);

(statearr_24664[(8)] = inst_24500);

(statearr_24664[(9)] = inst_24502);

(statearr_24664[(10)] = inst_24499);

return statearr_24664;
})();
var statearr_24665_24746 = state_24638__$1;
(statearr_24665_24746[(2)] = null);

(statearr_24665_24746[(1)] = (5));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (22))){
var inst_24552 = (state_24638[(19)]);
var inst_24560 = (state_24638[(23)]);
var inst_24556 = (state_24638[(24)]);
var inst_24555 = (state_24638[(25)]);
var inst_24555__$1 = (state_24638[(2)]);
var inst_24556__$1 = cljs.core.filter.call(null,new cljs.core.Keyword(null,"loaded-file","loaded-file",-168399375),inst_24555__$1);
var inst_24557 = (function (){var all_files = inst_24552;
var res_SINGLEQUOTE_ = inst_24555__$1;
var res = inst_24556__$1;
return ((function (all_files,res_SINGLEQUOTE_,res,inst_24552,inst_24560,inst_24556,inst_24555,inst_24555__$1,inst_24556__$1,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents){
return (function (p1__24235_SHARP_){
return cljs.core.not.call(null,new cljs.core.Keyword(null,"loaded-file","loaded-file",-168399375).cljs$core$IFn$_invoke$arity$1(p1__24235_SHARP_));
});
;})(all_files,res_SINGLEQUOTE_,res,inst_24552,inst_24560,inst_24556,inst_24555,inst_24555__$1,inst_24556__$1,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents))
})();
var inst_24558 = cljs.core.filter.call(null,inst_24557,inst_24555__$1);
var inst_24559 = cljs.core.deref.call(null,figwheel.client.file_reloading.dependencies_loaded);
var inst_24560__$1 = cljs.core.filter.call(null,new cljs.core.Keyword(null,"loaded-file","loaded-file",-168399375),inst_24559);
var inst_24561 = cljs.core.not_empty.call(null,inst_24560__$1);
var state_24638__$1 = (function (){var statearr_24666 = state_24638;
(statearr_24666[(26)] = inst_24558);

(statearr_24666[(23)] = inst_24560__$1);

(statearr_24666[(24)] = inst_24556__$1);

(statearr_24666[(25)] = inst_24555__$1);

return statearr_24666;
})();
if(cljs.core.truth_(inst_24561)){
var statearr_24667_24747 = state_24638__$1;
(statearr_24667_24747[(1)] = (23));

} else {
var statearr_24668_24748 = state_24638__$1;
(statearr_24668_24748[(1)] = (24));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (36))){
var state_24638__$1 = state_24638;
var statearr_24669_24749 = state_24638__$1;
(statearr_24669_24749[(2)] = false);

(statearr_24669_24749[(1)] = (37));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (41))){
var inst_24613 = (state_24638[(20)]);
var inst_24617 = cljs.core.comp.call(null,figwheel.client.file_reloading.name__GT_path,new cljs.core.Keyword(null,"namespace","namespace",-377510372));
var inst_24618 = cljs.core.map.call(null,inst_24617,inst_24613);
var inst_24619 = cljs.core.pr_str.call(null,inst_24618);
var inst_24620 = [cljs.core.str("figwheel-no-load meta-data: "),cljs.core.str(inst_24619)].join('');
var inst_24621 = figwheel.client.utils.log.call(null,inst_24620);
var state_24638__$1 = state_24638;
var statearr_24670_24750 = state_24638__$1;
(statearr_24670_24750[(2)] = inst_24621);

(statearr_24670_24750[(1)] = (43));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (43))){
var inst_24614 = (state_24638[(21)]);
var inst_24624 = (state_24638[(2)]);
var inst_24625 = cljs.core.not_empty.call(null,inst_24614);
var state_24638__$1 = (function (){var statearr_24671 = state_24638;
(statearr_24671[(27)] = inst_24624);

return statearr_24671;
})();
if(cljs.core.truth_(inst_24625)){
var statearr_24672_24751 = state_24638__$1;
(statearr_24672_24751[(1)] = (44));

} else {
var statearr_24673_24752 = state_24638__$1;
(statearr_24673_24752[(1)] = (45));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (29))){
var inst_24552 = (state_24638[(19)]);
var inst_24558 = (state_24638[(26)]);
var inst_24592 = (state_24638[(16)]);
var inst_24560 = (state_24638[(23)]);
var inst_24556 = (state_24638[(24)]);
var inst_24555 = (state_24638[(25)]);
var inst_24588 = figwheel.client.utils.log.call(null,new cljs.core.Keyword(null,"debug","debug",-1608172596),"Figwheel: NOT loading these files ");
var inst_24591 = (function (){var all_files = inst_24552;
var res_SINGLEQUOTE_ = inst_24555;
var res = inst_24556;
var files_not_loaded = inst_24558;
var dependencies_that_loaded = inst_24560;
return ((function (all_files,res_SINGLEQUOTE_,res,files_not_loaded,dependencies_that_loaded,inst_24552,inst_24558,inst_24592,inst_24560,inst_24556,inst_24555,inst_24588,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents){
return (function (p__24590){
var map__24674 = p__24590;
var map__24674__$1 = ((((!((map__24674 == null)))?((((map__24674.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24674.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24674):map__24674);
var namespace = cljs.core.get.call(null,map__24674__$1,new cljs.core.Keyword(null,"namespace","namespace",-377510372));
var meta_data = cljs.core.get.call(null,cljs.core.deref.call(null,figwheel.client.file_reloading.figwheel_meta_pragmas),cljs.core.name.call(null,namespace));
if((meta_data == null)){
return new cljs.core.Keyword(null,"not-required","not-required",-950359114);
} else {
if(cljs.core.truth_(meta_data.call(null,new cljs.core.Keyword(null,"figwheel-no-load","figwheel-no-load",-555840179)))){
return new cljs.core.Keyword(null,"figwheel-no-load","figwheel-no-load",-555840179);
} else {
return new cljs.core.Keyword(null,"not-required","not-required",-950359114);

}
}
});
;})(all_files,res_SINGLEQUOTE_,res,files_not_loaded,dependencies_that_loaded,inst_24552,inst_24558,inst_24592,inst_24560,inst_24556,inst_24555,inst_24588,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents))
})();
var inst_24592__$1 = cljs.core.group_by.call(null,inst_24591,inst_24558);
var inst_24594 = (inst_24592__$1 == null);
var inst_24595 = cljs.core.not.call(null,inst_24594);
var state_24638__$1 = (function (){var statearr_24676 = state_24638;
(statearr_24676[(16)] = inst_24592__$1);

(statearr_24676[(28)] = inst_24588);

return statearr_24676;
})();
if(inst_24595){
var statearr_24677_24753 = state_24638__$1;
(statearr_24677_24753[(1)] = (32));

} else {
var statearr_24678_24754 = state_24638__$1;
(statearr_24678_24754[(1)] = (33));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (44))){
var inst_24614 = (state_24638[(21)]);
var inst_24627 = cljs.core.map.call(null,new cljs.core.Keyword(null,"file","file",-1269645878),inst_24614);
var inst_24628 = cljs.core.pr_str.call(null,inst_24627);
var inst_24629 = [cljs.core.str("not required: "),cljs.core.str(inst_24628)].join('');
var inst_24630 = figwheel.client.utils.log.call(null,inst_24629);
var state_24638__$1 = state_24638;
var statearr_24679_24755 = state_24638__$1;
(statearr_24679_24755[(2)] = inst_24630);

(statearr_24679_24755[(1)] = (46));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (6))){
var inst_24533 = (state_24638[(2)]);
var state_24638__$1 = state_24638;
var statearr_24680_24756 = state_24638__$1;
(statearr_24680_24756[(2)] = inst_24533);

(statearr_24680_24756[(1)] = (4));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (28))){
var inst_24558 = (state_24638[(26)]);
var inst_24585 = (state_24638[(2)]);
var inst_24586 = cljs.core.not_empty.call(null,inst_24558);
var state_24638__$1 = (function (){var statearr_24681 = state_24638;
(statearr_24681[(29)] = inst_24585);

return statearr_24681;
})();
if(cljs.core.truth_(inst_24586)){
var statearr_24682_24757 = state_24638__$1;
(statearr_24682_24757[(1)] = (29));

} else {
var statearr_24683_24758 = state_24638__$1;
(statearr_24683_24758[(1)] = (30));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (25))){
var inst_24556 = (state_24638[(24)]);
var inst_24572 = (state_24638[(2)]);
var inst_24573 = cljs.core.not_empty.call(null,inst_24556);
var state_24638__$1 = (function (){var statearr_24684 = state_24638;
(statearr_24684[(30)] = inst_24572);

return statearr_24684;
})();
if(cljs.core.truth_(inst_24573)){
var statearr_24685_24759 = state_24638__$1;
(statearr_24685_24759[(1)] = (26));

} else {
var statearr_24686_24760 = state_24638__$1;
(statearr_24686_24760[(1)] = (27));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (34))){
var inst_24607 = (state_24638[(2)]);
var state_24638__$1 = state_24638;
if(cljs.core.truth_(inst_24607)){
var statearr_24687_24761 = state_24638__$1;
(statearr_24687_24761[(1)] = (38));

} else {
var statearr_24688_24762 = state_24638__$1;
(statearr_24688_24762[(1)] = (39));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (17))){
var state_24638__$1 = state_24638;
var statearr_24689_24763 = state_24638__$1;
(statearr_24689_24763[(2)] = recompile_dependents);

(statearr_24689_24763[(1)] = (18));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (3))){
var state_24638__$1 = state_24638;
var statearr_24690_24764 = state_24638__$1;
(statearr_24690_24764[(2)] = null);

(statearr_24690_24764[(1)] = (4));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (12))){
var inst_24529 = (state_24638[(2)]);
var state_24638__$1 = state_24638;
var statearr_24691_24765 = state_24638__$1;
(statearr_24691_24765[(2)] = inst_24529);

(statearr_24691_24765[(1)] = (9));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (2))){
var inst_24491 = (state_24638[(13)]);
var inst_24498 = cljs.core.seq.call(null,inst_24491);
var inst_24499 = inst_24498;
var inst_24500 = null;
var inst_24501 = (0);
var inst_24502 = (0);
var state_24638__$1 = (function (){var statearr_24692 = state_24638;
(statearr_24692[(7)] = inst_24501);

(statearr_24692[(8)] = inst_24500);

(statearr_24692[(9)] = inst_24502);

(statearr_24692[(10)] = inst_24499);

return statearr_24692;
})();
var statearr_24693_24766 = state_24638__$1;
(statearr_24693_24766[(2)] = null);

(statearr_24693_24766[(1)] = (5));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (23))){
var inst_24552 = (state_24638[(19)]);
var inst_24558 = (state_24638[(26)]);
var inst_24560 = (state_24638[(23)]);
var inst_24556 = (state_24638[(24)]);
var inst_24555 = (state_24638[(25)]);
var inst_24563 = figwheel.client.utils.log.call(null,new cljs.core.Keyword(null,"debug","debug",-1608172596),"Figwheel: loaded these dependencies");
var inst_24565 = (function (){var all_files = inst_24552;
var res_SINGLEQUOTE_ = inst_24555;
var res = inst_24556;
var files_not_loaded = inst_24558;
var dependencies_that_loaded = inst_24560;
return ((function (all_files,res_SINGLEQUOTE_,res,files_not_loaded,dependencies_that_loaded,inst_24552,inst_24558,inst_24560,inst_24556,inst_24555,inst_24563,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents){
return (function (p__24564){
var map__24694 = p__24564;
var map__24694__$1 = ((((!((map__24694 == null)))?((((map__24694.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24694.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24694):map__24694);
var request_url = cljs.core.get.call(null,map__24694__$1,new cljs.core.Keyword(null,"request-url","request-url",2100346596));
return clojure.string.replace.call(null,request_url,goog.basePath,"");
});
;})(all_files,res_SINGLEQUOTE_,res,files_not_loaded,dependencies_that_loaded,inst_24552,inst_24558,inst_24560,inst_24556,inst_24555,inst_24563,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents))
})();
var inst_24566 = cljs.core.reverse.call(null,inst_24560);
var inst_24567 = cljs.core.map.call(null,inst_24565,inst_24566);
var inst_24568 = cljs.core.pr_str.call(null,inst_24567);
var inst_24569 = figwheel.client.utils.log.call(null,inst_24568);
var state_24638__$1 = (function (){var statearr_24696 = state_24638;
(statearr_24696[(31)] = inst_24563);

return statearr_24696;
})();
var statearr_24697_24767 = state_24638__$1;
(statearr_24697_24767[(2)] = inst_24569);

(statearr_24697_24767[(1)] = (25));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (35))){
var state_24638__$1 = state_24638;
var statearr_24698_24768 = state_24638__$1;
(statearr_24698_24768[(2)] = true);

(statearr_24698_24768[(1)] = (37));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (19))){
var inst_24542 = (state_24638[(12)]);
var inst_24548 = figwheel.client.file_reloading.expand_files.call(null,inst_24542);
var state_24638__$1 = state_24638;
var statearr_24699_24769 = state_24638__$1;
(statearr_24699_24769[(2)] = inst_24548);

(statearr_24699_24769[(1)] = (21));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (11))){
var state_24638__$1 = state_24638;
var statearr_24700_24770 = state_24638__$1;
(statearr_24700_24770[(2)] = null);

(statearr_24700_24770[(1)] = (12));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (9))){
var inst_24531 = (state_24638[(2)]);
var state_24638__$1 = state_24638;
var statearr_24701_24771 = state_24638__$1;
(statearr_24701_24771[(2)] = inst_24531);

(statearr_24701_24771[(1)] = (6));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (5))){
var inst_24501 = (state_24638[(7)]);
var inst_24502 = (state_24638[(9)]);
var inst_24504 = (inst_24502 < inst_24501);
var inst_24505 = inst_24504;
var state_24638__$1 = state_24638;
if(cljs.core.truth_(inst_24505)){
var statearr_24702_24772 = state_24638__$1;
(statearr_24702_24772[(1)] = (7));

} else {
var statearr_24703_24773 = state_24638__$1;
(statearr_24703_24773[(1)] = (8));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (14))){
var inst_24512 = (state_24638[(22)]);
var inst_24521 = cljs.core.first.call(null,inst_24512);
var inst_24522 = figwheel.client.file_reloading.eval_body.call(null,inst_24521,opts);
var inst_24523 = cljs.core.next.call(null,inst_24512);
var inst_24499 = inst_24523;
var inst_24500 = null;
var inst_24501 = (0);
var inst_24502 = (0);
var state_24638__$1 = (function (){var statearr_24704 = state_24638;
(statearr_24704[(32)] = inst_24522);

(statearr_24704[(7)] = inst_24501);

(statearr_24704[(8)] = inst_24500);

(statearr_24704[(9)] = inst_24502);

(statearr_24704[(10)] = inst_24499);

return statearr_24704;
})();
var statearr_24705_24774 = state_24638__$1;
(statearr_24705_24774[(2)] = null);

(statearr_24705_24774[(1)] = (5));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (45))){
var state_24638__$1 = state_24638;
var statearr_24706_24775 = state_24638__$1;
(statearr_24706_24775[(2)] = null);

(statearr_24706_24775[(1)] = (46));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (26))){
var inst_24552 = (state_24638[(19)]);
var inst_24558 = (state_24638[(26)]);
var inst_24560 = (state_24638[(23)]);
var inst_24556 = (state_24638[(24)]);
var inst_24555 = (state_24638[(25)]);
var inst_24575 = figwheel.client.utils.log.call(null,new cljs.core.Keyword(null,"debug","debug",-1608172596),"Figwheel: loaded these files");
var inst_24577 = (function (){var all_files = inst_24552;
var res_SINGLEQUOTE_ = inst_24555;
var res = inst_24556;
var files_not_loaded = inst_24558;
var dependencies_that_loaded = inst_24560;
return ((function (all_files,res_SINGLEQUOTE_,res,files_not_loaded,dependencies_that_loaded,inst_24552,inst_24558,inst_24560,inst_24556,inst_24555,inst_24575,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents){
return (function (p__24576){
var map__24707 = p__24576;
var map__24707__$1 = ((((!((map__24707 == null)))?((((map__24707.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24707.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24707):map__24707);
var namespace = cljs.core.get.call(null,map__24707__$1,new cljs.core.Keyword(null,"namespace","namespace",-377510372));
var file = cljs.core.get.call(null,map__24707__$1,new cljs.core.Keyword(null,"file","file",-1269645878));
if(cljs.core.truth_(namespace)){
return figwheel.client.file_reloading.name__GT_path.call(null,cljs.core.name.call(null,namespace));
} else {
return file;
}
});
;})(all_files,res_SINGLEQUOTE_,res,files_not_loaded,dependencies_that_loaded,inst_24552,inst_24558,inst_24560,inst_24556,inst_24555,inst_24575,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents))
})();
var inst_24578 = cljs.core.map.call(null,inst_24577,inst_24556);
var inst_24579 = cljs.core.pr_str.call(null,inst_24578);
var inst_24580 = figwheel.client.utils.log.call(null,inst_24579);
var inst_24581 = (function (){var all_files = inst_24552;
var res_SINGLEQUOTE_ = inst_24555;
var res = inst_24556;
var files_not_loaded = inst_24558;
var dependencies_that_loaded = inst_24560;
return ((function (all_files,res_SINGLEQUOTE_,res,files_not_loaded,dependencies_that_loaded,inst_24552,inst_24558,inst_24560,inst_24556,inst_24555,inst_24575,inst_24577,inst_24578,inst_24579,inst_24580,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents){
return (function (){
figwheel.client.file_reloading.on_jsload_custom_event.call(null,res);

return cljs.core.apply.call(null,on_jsload,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [res], null));
});
;})(all_files,res_SINGLEQUOTE_,res,files_not_loaded,dependencies_that_loaded,inst_24552,inst_24558,inst_24560,inst_24556,inst_24555,inst_24575,inst_24577,inst_24578,inst_24579,inst_24580,state_val_24639,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents))
})();
var inst_24582 = setTimeout(inst_24581,(10));
var state_24638__$1 = (function (){var statearr_24709 = state_24638;
(statearr_24709[(33)] = inst_24575);

(statearr_24709[(34)] = inst_24580);

return statearr_24709;
})();
var statearr_24710_24776 = state_24638__$1;
(statearr_24710_24776[(2)] = inst_24582);

(statearr_24710_24776[(1)] = (28));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (16))){
var state_24638__$1 = state_24638;
var statearr_24711_24777 = state_24638__$1;
(statearr_24711_24777[(2)] = reload_dependents);

(statearr_24711_24777[(1)] = (18));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (38))){
var inst_24592 = (state_24638[(16)]);
var inst_24609 = cljs.core.apply.call(null,cljs.core.hash_map,inst_24592);
var state_24638__$1 = state_24638;
var statearr_24712_24778 = state_24638__$1;
(statearr_24712_24778[(2)] = inst_24609);

(statearr_24712_24778[(1)] = (40));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (30))){
var state_24638__$1 = state_24638;
var statearr_24713_24779 = state_24638__$1;
(statearr_24713_24779[(2)] = null);

(statearr_24713_24779[(1)] = (31));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (10))){
var inst_24512 = (state_24638[(22)]);
var inst_24514 = cljs.core.chunked_seq_QMARK_.call(null,inst_24512);
var state_24638__$1 = state_24638;
if(inst_24514){
var statearr_24714_24780 = state_24638__$1;
(statearr_24714_24780[(1)] = (13));

} else {
var statearr_24715_24781 = state_24638__$1;
(statearr_24715_24781[(1)] = (14));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (18))){
var inst_24546 = (state_24638[(2)]);
var state_24638__$1 = state_24638;
if(cljs.core.truth_(inst_24546)){
var statearr_24716_24782 = state_24638__$1;
(statearr_24716_24782[(1)] = (19));

} else {
var statearr_24717_24783 = state_24638__$1;
(statearr_24717_24783[(1)] = (20));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (42))){
var state_24638__$1 = state_24638;
var statearr_24718_24784 = state_24638__$1;
(statearr_24718_24784[(2)] = null);

(statearr_24718_24784[(1)] = (43));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (37))){
var inst_24604 = (state_24638[(2)]);
var state_24638__$1 = state_24638;
var statearr_24719_24785 = state_24638__$1;
(statearr_24719_24785[(2)] = inst_24604);

(statearr_24719_24785[(1)] = (34));


return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
if((state_val_24639 === (8))){
var inst_24512 = (state_24638[(22)]);
var inst_24499 = (state_24638[(10)]);
var inst_24512__$1 = cljs.core.seq.call(null,inst_24499);
var state_24638__$1 = (function (){var statearr_24720 = state_24638;
(statearr_24720[(22)] = inst_24512__$1);

return statearr_24720;
})();
if(inst_24512__$1){
var statearr_24721_24786 = state_24638__$1;
(statearr_24721_24786[(1)] = (10));

} else {
var statearr_24722_24787 = state_24638__$1;
(statearr_24722_24787[(1)] = (11));

}

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
return null;
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
}
});})(c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents))
;
return ((function (switch__20838__auto__,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents){
return (function() {
var figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto__ = null;
var figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto____0 = (function (){
var statearr_24726 = [null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null];
(statearr_24726[(0)] = figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto__);

(statearr_24726[(1)] = (1));

return statearr_24726;
});
var figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto____1 = (function (state_24638){
while(true){
var ret_value__20840__auto__ = (function (){try{while(true){
var result__20841__auto__ = switch__20838__auto__.call(null,state_24638);
if(cljs.core.keyword_identical_QMARK_.call(null,result__20841__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
continue;
} else {
return result__20841__auto__;
}
break;
}
}catch (e24727){if((e24727 instanceof Object)){
var ex__20842__auto__ = e24727;
var statearr_24728_24788 = state_24638;
(statearr_24728_24788[(5)] = ex__20842__auto__);


cljs.core.async.impl.ioc_helpers.process_exception.call(null,state_24638);

return new cljs.core.Keyword(null,"recur","recur",-437573268);
} else {
throw e24727;

}
}})();
if(cljs.core.keyword_identical_QMARK_.call(null,ret_value__20840__auto__,new cljs.core.Keyword(null,"recur","recur",-437573268))){
var G__24789 = state_24638;
state_24638 = G__24789;
continue;
} else {
return ret_value__20840__auto__;
}
break;
}
});
figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto__ = function(state_24638){
switch(arguments.length){
case 0:
return figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto____0.call(this);
case 1:
return figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto____1.call(this,state_24638);
}
throw(new Error('Invalid arity: ' + arguments.length));
};
figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto__.cljs$core$IFn$_invoke$arity$0 = figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto____0;
figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto__.cljs$core$IFn$_invoke$arity$1 = figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto____1;
return figwheel$client$file_reloading$reload_js_files_$_state_machine__20839__auto__;
})()
;})(switch__20838__auto__,c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents))
})();
var state__20952__auto__ = (function (){var statearr_24729 = f__20951__auto__.call(null);
(statearr_24729[cljs.core.async.impl.ioc_helpers.USER_START_IDX] = c__20950__auto__);

return statearr_24729;
})();
return cljs.core.async.impl.ioc_helpers.run_state_machine_wrapped.call(null,state__20952__auto__);
});})(c__20950__auto__,map__24484,map__24484__$1,opts,before_jsload,on_jsload,reload_dependents,map__24485,map__24485__$1,msg,files,figwheel_meta,recompile_dependents))
);

return c__20950__auto__;
});
figwheel.client.file_reloading.current_links = (function figwheel$client$file_reloading$current_links(){
return Array.prototype.slice.call(document.getElementsByTagName("link"));
});
figwheel.client.file_reloading.truncate_url = (function figwheel$client$file_reloading$truncate_url(url){
return clojure.string.replace_first.call(null,clojure.string.replace_first.call(null,clojure.string.replace_first.call(null,clojure.string.replace_first.call(null,cljs.core.first.call(null,clojure.string.split.call(null,url,/\?/)),[cljs.core.str(location.protocol),cljs.core.str("//")].join(''),""),".*://",""),/^\/\//,""),/[^\\/]*/,"");
});
figwheel.client.file_reloading.matches_file_QMARK_ = (function figwheel$client$file_reloading$matches_file_QMARK_(p__24792,link){
var map__24795 = p__24792;
var map__24795__$1 = ((((!((map__24795 == null)))?((((map__24795.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24795.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24795):map__24795);
var file = cljs.core.get.call(null,map__24795__$1,new cljs.core.Keyword(null,"file","file",-1269645878));
var temp__4425__auto__ = link.href;
if(cljs.core.truth_(temp__4425__auto__)){
var link_href = temp__4425__auto__;
var match = clojure.string.join.call(null,"/",cljs.core.take_while.call(null,cljs.core.identity,cljs.core.map.call(null,((function (link_href,temp__4425__auto__,map__24795,map__24795__$1,file){
return (function (p1__24790_SHARP_,p2__24791_SHARP_){
if(cljs.core._EQ_.call(null,p1__24790_SHARP_,p2__24791_SHARP_)){
return p1__24790_SHARP_;
} else {
return false;
}
});})(link_href,temp__4425__auto__,map__24795,map__24795__$1,file))
,cljs.core.reverse.call(null,clojure.string.split.call(null,file,"/")),cljs.core.reverse.call(null,clojure.string.split.call(null,figwheel.client.file_reloading.truncate_url.call(null,link_href),"/")))));
var match_length = cljs.core.count.call(null,match);
var file_name_length = cljs.core.count.call(null,cljs.core.last.call(null,clojure.string.split.call(null,file,"/")));
if((match_length >= file_name_length)){
return new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"link","link",-1769163468),link,new cljs.core.Keyword(null,"link-href","link-href",-250644450),link_href,new cljs.core.Keyword(null,"match-length","match-length",1101537310),match_length,new cljs.core.Keyword(null,"current-url-length","current-url-length",380404083),cljs.core.count.call(null,figwheel.client.file_reloading.truncate_url.call(null,link_href))], null);
} else {
return null;
}
} else {
return null;
}
});
figwheel.client.file_reloading.get_correct_link = (function figwheel$client$file_reloading$get_correct_link(f_data){
var temp__4425__auto__ = cljs.core.first.call(null,cljs.core.sort_by.call(null,(function (p__24801){
var map__24802 = p__24801;
var map__24802__$1 = ((((!((map__24802 == null)))?((((map__24802.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24802.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24802):map__24802);
var match_length = cljs.core.get.call(null,map__24802__$1,new cljs.core.Keyword(null,"match-length","match-length",1101537310));
var current_url_length = cljs.core.get.call(null,map__24802__$1,new cljs.core.Keyword(null,"current-url-length","current-url-length",380404083));
return (current_url_length - match_length);
}),cljs.core.keep.call(null,(function (p1__24797_SHARP_){
return figwheel.client.file_reloading.matches_file_QMARK_.call(null,f_data,p1__24797_SHARP_);
}),figwheel.client.file_reloading.current_links.call(null))));
if(cljs.core.truth_(temp__4425__auto__)){
var res = temp__4425__auto__;
return new cljs.core.Keyword(null,"link","link",-1769163468).cljs$core$IFn$_invoke$arity$1(res);
} else {
return null;
}
});
figwheel.client.file_reloading.clone_link = (function figwheel$client$file_reloading$clone_link(link,url){
var clone = document.createElement("link");
clone.rel = "stylesheet";

clone.media = link.media;

clone.disabled = link.disabled;

clone.href = figwheel.client.file_reloading.add_cache_buster.call(null,url);

return clone;
});
figwheel.client.file_reloading.create_link = (function figwheel$client$file_reloading$create_link(url){
var link = document.createElement("link");
link.rel = "stylesheet";

link.href = figwheel.client.file_reloading.add_cache_buster.call(null,url);

return link;
});
figwheel.client.file_reloading.add_link_to_doc = (function figwheel$client$file_reloading$add_link_to_doc(var_args){
var args24804 = [];
var len__17884__auto___24807 = arguments.length;
var i__17885__auto___24808 = (0);
while(true){
if((i__17885__auto___24808 < len__17884__auto___24807)){
args24804.push((arguments[i__17885__auto___24808]));

var G__24809 = (i__17885__auto___24808 + (1));
i__17885__auto___24808 = G__24809;
continue;
} else {
}
break;
}

var G__24806 = args24804.length;
switch (G__24806) {
case 1:
return figwheel.client.file_reloading.add_link_to_doc.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return figwheel.client.file_reloading.add_link_to_doc.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error([cljs.core.str("Invalid arity: "),cljs.core.str(args24804.length)].join('')));

}
});

figwheel.client.file_reloading.add_link_to_doc.cljs$core$IFn$_invoke$arity$1 = (function (new_link){
return (document.getElementsByTagName("head")[(0)]).appendChild(new_link);
});

figwheel.client.file_reloading.add_link_to_doc.cljs$core$IFn$_invoke$arity$2 = (function (orig_link,klone){
var parent = orig_link.parentNode;
if(cljs.core._EQ_.call(null,orig_link,parent.lastChild)){
parent.appendChild(klone);
} else {
parent.insertBefore(klone,orig_link.nextSibling);
}

return setTimeout(((function (parent){
return (function (){
return parent.removeChild(orig_link);
});})(parent))
,(300));
});

figwheel.client.file_reloading.add_link_to_doc.cljs$lang$maxFixedArity = 2;
figwheel.client.file_reloading.distictify = (function figwheel$client$file_reloading$distictify(key,seqq){
return cljs.core.vals.call(null,cljs.core.reduce.call(null,(function (p1__24811_SHARP_,p2__24812_SHARP_){
return cljs.core.assoc.call(null,p1__24811_SHARP_,cljs.core.get.call(null,p2__24812_SHARP_,key),p2__24812_SHARP_);
}),cljs.core.PersistentArrayMap.EMPTY,seqq));
});
figwheel.client.file_reloading.reload_css_file = (function figwheel$client$file_reloading$reload_css_file(p__24813){
var map__24816 = p__24813;
var map__24816__$1 = ((((!((map__24816 == null)))?((((map__24816.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24816.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24816):map__24816);
var f_data = map__24816__$1;
var file = cljs.core.get.call(null,map__24816__$1,new cljs.core.Keyword(null,"file","file",-1269645878));
var temp__4425__auto__ = figwheel.client.file_reloading.get_correct_link.call(null,f_data);
if(cljs.core.truth_(temp__4425__auto__)){
var link = temp__4425__auto__;
return figwheel.client.file_reloading.add_link_to_doc.call(null,link,figwheel.client.file_reloading.clone_link.call(null,link,link.href));
} else {
return null;
}
});
figwheel.client.file_reloading.reload_css_files = (function figwheel$client$file_reloading$reload_css_files(p__24818,files_msg){
var map__24825 = p__24818;
var map__24825__$1 = ((((!((map__24825 == null)))?((((map__24825.cljs$lang$protocol_mask$partition0$ & (64))) || (map__24825.cljs$core$ISeq$))?true:false):false))?cljs.core.apply.call(null,cljs.core.hash_map,map__24825):map__24825);
var opts = map__24825__$1;
var on_cssload = cljs.core.get.call(null,map__24825__$1,new cljs.core.Keyword(null,"on-cssload","on-cssload",1825432318));
if(cljs.core.truth_(figwheel.client.utils.html_env_QMARK_.call(null))){
var seq__24827_24831 = cljs.core.seq.call(null,figwheel.client.file_reloading.distictify.call(null,new cljs.core.Keyword(null,"file","file",-1269645878),new cljs.core.Keyword(null,"files","files",-472457450).cljs$core$IFn$_invoke$arity$1(files_msg)));
var chunk__24828_24832 = null;
var count__24829_24833 = (0);
var i__24830_24834 = (0);
while(true){
if((i__24830_24834 < count__24829_24833)){
var f_24835 = cljs.core._nth.call(null,chunk__24828_24832,i__24830_24834);
figwheel.client.file_reloading.reload_css_file.call(null,f_24835);

var G__24836 = seq__24827_24831;
var G__24837 = chunk__24828_24832;
var G__24838 = count__24829_24833;
var G__24839 = (i__24830_24834 + (1));
seq__24827_24831 = G__24836;
chunk__24828_24832 = G__24837;
count__24829_24833 = G__24838;
i__24830_24834 = G__24839;
continue;
} else {
var temp__4425__auto___24840 = cljs.core.seq.call(null,seq__24827_24831);
if(temp__4425__auto___24840){
var seq__24827_24841__$1 = temp__4425__auto___24840;
if(cljs.core.chunked_seq_QMARK_.call(null,seq__24827_24841__$1)){
var c__17629__auto___24842 = cljs.core.chunk_first.call(null,seq__24827_24841__$1);
var G__24843 = cljs.core.chunk_rest.call(null,seq__24827_24841__$1);
var G__24844 = c__17629__auto___24842;
var G__24845 = cljs.core.count.call(null,c__17629__auto___24842);
var G__24846 = (0);
seq__24827_24831 = G__24843;
chunk__24828_24832 = G__24844;
count__24829_24833 = G__24845;
i__24830_24834 = G__24846;
continue;
} else {
var f_24847 = cljs.core.first.call(null,seq__24827_24841__$1);
figwheel.client.file_reloading.reload_css_file.call(null,f_24847);

var G__24848 = cljs.core.next.call(null,seq__24827_24841__$1);
var G__24849 = null;
var G__24850 = (0);
var G__24851 = (0);
seq__24827_24831 = G__24848;
chunk__24828_24832 = G__24849;
count__24829_24833 = G__24850;
i__24830_24834 = G__24851;
continue;
}
} else {
}
}
break;
}

return setTimeout(((function (map__24825,map__24825__$1,opts,on_cssload){
return (function (){
return on_cssload.call(null,new cljs.core.Keyword(null,"files","files",-472457450).cljs$core$IFn$_invoke$arity$1(files_msg));
});})(map__24825,map__24825__$1,opts,on_cssload))
,(100));
} else {
return null;
}
});

//# sourceMappingURL=file_reloading.js.map?rel=1449036926031