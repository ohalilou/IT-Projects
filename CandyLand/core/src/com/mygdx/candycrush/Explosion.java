package com.mygdx.candycrush;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.Animation;
import com.badlogic.gdx.graphics.g2d.TextureRegion;


// 
public class Explosion {

    public static Texture bounce_sprite;
    private static final int COLS = 10;
    private static final int ROWS = 7;

    private Animation<TextureRegion> bounce_animation;
    static TextureRegion[] bounce_frames;
    private TextureRegion current_frame;
    private float state_time;
    final CandyCrush game;
    final float x;
    final float y;
    final float size;

    static List<Explosion> explosions = new ArrayList<Explosion>(); 


    public Explosion(final CandyCrush game,final float x,final float y,final float size) {
        this.game = game;
        this.x = x;
        this.y = y;
        this.size = size;
        explosions.add(this);
        bounce_sprite = new Texture(Gdx.files.internal("spritesheet.png"));
        TextureRegion[][] tmp = TextureRegion.split(bounce_sprite, bounce_sprite.getWidth()/COLS, bounce_sprite.getHeight()/ROWS);
        bounce_frames = new TextureRegion[COLS * ROWS];
        int index = 0;
        for(int i=0; i<ROWS; ++i) {
            for(int j=0; j<COLS; ++j) {
                bounce_frames[index++] = tmp[i][j];
            }
        }

        bounce_animation = new Animation<TextureRegion>(0.016f, bounce_frames);
        state_time = 0f;

    }

    private void animate(float delta, float x, float y,float size) {
        state_time += delta;
        current_frame = bounce_animation.getKeyFrame(state_time, false);
        game.batch.draw(current_frame,x,y,size,size);
    }

    static void animateExplosions(float delta) {

        for (Iterator<Explosion> iterator = explosions.iterator(); iterator.hasNext();) {
            Explosion explo = iterator.next();
            if (explo.bounce_animation.isAnimationFinished(explo.state_time)) {
                iterator.remove();
            } else {
                explo.animate(delta,explo.x,explo.y,explo.size);
            }
        }
        
    }
    
}
