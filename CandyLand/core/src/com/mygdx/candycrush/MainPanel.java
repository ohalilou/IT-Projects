package com.mygdx.candycrush;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Screen;

import com.badlogic.gdx.graphics.OrthographicCamera;

import com.badlogic.gdx.graphics.Texture;

import com.badlogic.gdx.graphics.g2d.TextureRegion;

import com.badlogic.gdx.scenes.scene2d.InputEvent;
import com.badlogic.gdx.scenes.scene2d.Stage;
import com.badlogic.gdx.scenes.scene2d.ui.Button;
import com.badlogic.gdx.scenes.scene2d.ui.ImageButton;

import com.badlogic.gdx.scenes.scene2d.utils.ClickListener;
import com.badlogic.gdx.scenes.scene2d.utils.TextureRegionDrawable;
import com.badlogic.gdx.utils.ScreenUtils;

public class MainPanel implements Screen {

    private Stage stage;
    final CandyCrush game;
    OrthographicCamera camera;
    private Texture backgroundImage;
    private Texture title;
    private Button playButton;
    private Button InstructionsButton;
    private Button SettingsButton;

    private void createPlayButton() {
        Texture buttonTexture = new Texture(Gdx.files.internal("PlayButton.png"));
        playButton = new ImageButton(new TextureRegionDrawable(new TextureRegion(buttonTexture)));
        playButton.setPosition(Gdx.graphics.getWidth()/16, Gdx.graphics.getHeight()/2);  
        playButton.setTransform(true);
        playButton.setScale(0.75f,0.8f);
        stage.addActor(playButton);
        

        playButton.addListener(new ClickListener() {
            @Override
            public void clicked(InputEvent event, float x, float y){
                game.setScreen(new GameScreen(game));
                dispose();
            }
        });
    }

    private void createInstructionsButton() {
        Texture buttonTexture = new Texture(Gdx.files.internal("InstructionsButton.png"));
        InstructionsButton = new ImageButton(new TextureRegionDrawable(new TextureRegion(buttonTexture)));
        InstructionsButton.setPosition(Gdx.graphics.getWidth()*8/16, Gdx.graphics.getHeight()/2);  
        InstructionsButton.setTransform(true);
        InstructionsButton.setScale(0.75f,0.8f);
        stage.addActor(InstructionsButton);
    }

    private void createSettingsButton() {
        Texture buttonTexture = new Texture(Gdx.files.internal("SettingsButton.png"));
        SettingsButton = new ImageButton(new TextureRegionDrawable(new TextureRegion(buttonTexture)));
        SettingsButton.setPosition(Gdx.graphics.getWidth()*4/15, Gdx.graphics.getHeight()*1/4);  
        SettingsButton.setTransform(true);
        SettingsButton.setScale(0.75f,0.8f);
        stage.addActor(SettingsButton);
    }

    public MainPanel(final CandyCrush game) {
        this.game = game;
        stage = new Stage();
        Gdx.input.setInputProcessor(stage);
        backgroundImage = new Texture(Gdx.files.internal("CandyLandBackGround.png"));
        title = new Texture(Gdx.files.internal("CandyLandTitle.png"));
        createPlayButton();
        createInstructionsButton();
        createSettingsButton();

        camera = new OrthographicCamera();
        camera.setToOrtho(false, 780, 900);
    }


    @Override
    public void render(float delta) {
        ScreenUtils.clear(0, 0, 0.2f, 1);
        camera.update();
		game.batch.setProjectionMatrix(camera.combined);

		game.batch.begin();
		game.batch.draw(backgroundImage, 0, 0, Gdx.graphics.getWidth(), Gdx.graphics.getHeight());
		game.batch.draw(title, 0, Gdx.graphics.getHeight()*3/4 , Gdx.graphics.getWidth(), title.getHeight());
        stage.draw();
		game.batch.end();     
    }


    @Override
    public void dispose() {
        stage.dispose();
    }
    
    
    @Override
    public void resize(int width, int height) {}

    @Override
    public void pause() {}

    @Override
    public void resume() {}

    @Override
    public void hide() {}

    @Override
    public void show() {}

}
