package org.foi.androidworkshop.adapters;

import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import org.foi.androidworkshop.R;
import org.foi.androidworkshop.models.Pokemon;

import java.util.List;

public class PokemonAdapter extends RecyclerView.Adapter<PokemonAdapter.ViewHolder> {

    private List<Pokemon> pokemons;

    public PokemonAdapter(List<Pokemon> pokemons) {
        this.pokemons = pokemons;
    }

    @Override
    public ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        return new ViewHolder(LayoutInflater.from(parent.getContext()).inflate(R.layout.pokemon_item, parent, false));
    }

    @Override
    public void onBindViewHolder(ViewHolder holder, int position) {
        holder.ivPokemonThumbnail.setImageResource(R.mipmap.ic_launcher);
        holder.tvPokeName.setText(pokemons.get(position).getName());
        holder.tvPokeUrl.setText(pokemons.get(position).getUrl());
    }

    @Override
    public int getItemCount() {
        return pokemons.size();
    }

    public class ViewHolder extends RecyclerView.ViewHolder {

        private ImageView ivPokemonThumbnail;
        private TextView tvPokeName;
        private TextView tvPokeUrl;

        public ViewHolder(View itemView) {
            super(itemView);

            ivPokemonThumbnail = (ImageView) itemView.findViewById(R.id.ivPokeThumbnail);
            tvPokeName = (TextView) itemView.findViewById(R.id.tvPokeName);
            tvPokeUrl = (TextView) itemView.findViewById(R.id.tvPokeUrl);
        }
    }
}
