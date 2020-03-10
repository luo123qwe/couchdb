% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


-module(mango_indexer).


-export([
    modify/5
]).


-include_lib("couch/include/couch_db.hrl").
-include("mango.hrl").
-include("mango_idx.hrl").

-include_lib("couch_mrview/include/couch_mrview.hrl").
-include_lib("couch_views/include/couch_views.hrl").


modify(Db, Change, Doc, PrevDoc, Seq) ->
    try
        Indexes = json_indexes(Db, Doc),
        modify_int(Db, Change, Doc, Indexes, Seq)
    catch
        Error:Reason ->
            DbName = fabric2_db:name(Db),
            io:format("ERROR ~p ~p ~p ~n", [Error, Reason, erlang:display(erlang:get_stacktrace())]),
            Id = doc_id(Doc, PrevDoc),
            couch_log:error("Mango index error for Db ~s Doc ~p ~p ~p",
                [DbName, Id, Error, Reason])
    end.


doc_id(undefined, #doc{id = DocId}) ->
    DocId;
doc_id(undefined, _) ->
    <<"unknown_doc_id">>;
doc_id(#doc{id = DocId}, _) ->
    DocId.


% Check if design doc is mango index and kick off background worker
% to build the new index
modify_int(Db, Change, #doc{id = <<?DESIGN_DOC_PREFIX, _/binary>>} = Doc,
        Indexes, Seq) when Change == created orelse Change == updated ->
    DbName = fabric2_db:name(Db),

    {Props} = couch_doc:to_json_obj(Doc, []),
    case proplists:get_value(<<"language">>, Props) of
        <<"query">> ->
            {ok, Mrst} = couch_mrview_util:ddoc_to_mrst(DbName, Doc),
            couch_views_fdb:create_build_vs(Db, Mrst),
            {ok, _} = couch_views_jobs:build_view_async(Db, Mrst, true);
        _ ->
            ok
    end,
    update_indexes_seq(Db, Indexes, Seq);

modify_int(Db, _Change, Doc, Indexes, Seq) ->
    write_doc(Db, Doc, Indexes, Seq).


write_doc(Db, #doc{deleted = Deleted} = Doc, Indexes, Seq) ->
    #doc{id = DocId} = Doc,
    JsonDoc = mango_json:to_binary(couch_doc:to_json_obj(Doc, [])),

    lists:foreach(fun (Idx) ->
        DocResult0 = #{
            id => DocId,
            results => []
        },

        DocResult1 = case Deleted of
            true ->
                DocResult0#{deleted => true};
            false ->
                Results = mango_eval:index_doc([Idx], JsonDoc),
                DocResult0#{results => Results}
        end,

        DbName = mango_idx:dbname(Idx),
        DDoc = mango_idx:ddoc(Idx),
        {ok, Mrst} = couch_mrview_util:ddoc_to_mrst(DbName, DDoc),
        couch_views_indexer:write_docs(Db, Mrst, [DocResult1], #{last_seq => Seq})
    end, Indexes).

json_indexes(Db,  #doc{id = <<?DESIGN_DOC_PREFIX, _/binary>> = Id}) ->
    json_indexes(Db, [{ignore, [Id]}]);

json_indexes(Db,  #doc{}) ->
    json_indexes(Db, []);

json_indexes(Db, Opts) when is_list(Opts) ->
    #{
        md_version := MdVersion
    } = Db,

    case get(json_indexes) of
        {SavedVersion, Indexes} when MdVersion == SavedVersion ->
            Indexes;
        _ ->
            Indexes = lists:filter(fun(Idx) ->
                Idx#idx.type == <<"json">>
            end, mango_idx:list(Db, Opts)),
            put(json_indexes, {MdVersion, Indexes}),
            Indexes
    end.


update_indexes_seq(Db, Indexes, Seq) ->
    lists:foreach(fun (Idx) ->
        if Idx#idx.build_status /= ?INDEX_READY -> ok; true ->
            DbName = mango_idx:dbname(Idx),
            DDoc = mango_idx:ddoc(Idx),
            {ok, Mrst} = couch_mrview_util:ddoc_to_mrst(DbName, DDoc),
            couch_views_fdb:set_vs_update_seq(Db, Mrst#mrst.sig, Seq)
        end
    end, Indexes).


