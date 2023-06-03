package com.mygdx.candycrush;

import com.badlogic.gdx.Screen;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Input.Buttons;

import com.badlogic.gdx.graphics.OrthographicCamera;
import com.badlogic.gdx.graphics.g2d.BitmapFont;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer.ShapeType;
import com.badlogic.gdx.math.Vector3;
import com.badlogic.gdx.utils.ScreenUtils;

public class GameScreen implements Screen {

    final CandyCrush game;
	final double c1 = 1.70158;
	final double c3 = c1 + 1;

    public GameScreen(final CandyCrush game) {
        this.game = game;

		firstCandySelected = null;
		movingCandies = new ArrayList<Bonbon>(); 

		// Setting up the camera 
		setupCamera();


		// setup sprite renderer
		renderer = new ShapeRenderer();

		// setup the grid
		grid = new Grille(10, 10);
		startBegginingAnimation();
    }

    private OrthographicCamera camera;

	BitmapFont font; 
	ShapeRenderer renderer;
	Grille grid;
	private Bonbon firstCandySelected;
	private Bonbon currentSelectedCandy;
	private List<Bonbon> movingCandies;
	final int CIRCLE_RAD = 30;
	final int SPEED = 5;
	static float state_time = 0;


	private double distanceFromCandyToMouse(Bonbon bb, Vector3 pos) {
		if (bb == null) return CIRCLE_RAD + 1;
		return Math.sqrt(Math.pow(bb.getC() - pos.x,2) + Math.pow(bb.getL() - pos.y,2));
	}

	/*
	private void PrintdistancefromMousetoOrigin(Vector3 pos) {
		System.out.println(Math.sqrt(Math.pow(pos.x,2) - Math.pow(pos.y,2)));
	}
	*/

	private Bonbon selectClosestCandyToMouse(Vector3 pos) {
		for (int i=0; i < grid.hauteur; i++) {
			for (int j=0; j < grid.largeur; j++) {
				if (distanceFromCandyToMouse(grid.grille[i][j],pos) <= CIRCLE_RAD) {
					return grid.grille[i][j];
				}
			}
		}
		return null;
	}

	private Vector3 getMousePosition() {
		Vector3 touchPos = new Vector3();
		touchPos.set(Gdx.input.getX(), Gdx.input.getY(), 0);
		camera.unproject(touchPos);
		return touchPos;
	}

	private void switchCandy(Bonbon b1, Bonbon b2) {
		grid.permuter(b1,b2);
		movingCandies.add(b1);
		movingCandies.add(b2);
	}


	private void setupCamera() {
		camera = new OrthographicCamera();
		camera.setToOrtho(false, 780, 900);
	}

	@Override
	public void render (float delta) {
		ScreenUtils.clear(0, 0, 0.2f, 1);
		camera.update();
		game.batch.setProjectionMatrix(camera.combined);
		
		
		//System.out.println(grid.grille[0][0].animX);
		if (!movingCandies.isEmpty()) {
			moveCandies();
		} else {
			if (firstCandySelected != null & currentSelectedCandy != null) {
				destroyNeighbors(currentSelectedCandy,grid.getNbsVoisins(currentSelectedCandy));
				destroyNeighbors(firstCandySelected,grid.getNbsVoisins(firstCandySelected));
				ArrayList<Integer> l = grid.detruitAliDouble(currentSelectedCandy,firstCandySelected);
				if (!l.isEmpty()) {
					for (int i : l) {
						descendreCol(i);
					}
				} else {
					switchCandy(firstCandySelected, currentSelectedCandy);
				}
				firstCandySelected = null;		
			}
		


			if (Gdx.input.isButtonJustPressed(Buttons.LEFT)) {
				Vector3 pos = getMousePosition();
				currentSelectedCandy = selectClosestCandyToMouse(pos);
				if (currentSelectedCandy != null) {
					if (firstCandySelected == null) {
						firstCandySelected = currentSelectedCandy;
						currentSelectedCandy = null;
						
					} else {
						if (grid.sontVoisins(currentSelectedCandy, firstCandySelected)) {
							switchCandy(currentSelectedCandy,firstCandySelected);
						
							
						}
					}
				}
			} else if (Gdx.input.isButtonJustPressed(Buttons.RIGHT)) {
				Vector3 posi = getMousePosition();
				Bonbon b1 = selectClosestCandyToMouse(posi);
				
				if (b1 != null) {
					ArrayList<Integer> voisin = grid.getNbsVoisins(b1);
					ArrayList<Integer> l = grid.detruitali(b1);
					if (!l.isEmpty()) {
						destroyNeighbors(b1,voisin);
						for (int i : l) {
							descendreCol(i);
						}
					}
				}
	
			}
		}

		game.batch.begin();
		renderer.begin(ShapeType.Filled);
		drawGrid();
		renderer.end();

		game.batch.end();
		
		game.batch.begin();
		Explosion.animateExplosions(delta);
		drawText();
		game.batch.end();
	}
	
	@Override
	public void dispose () {
		renderer.dispose();
	}


	private void drawGrid() {
		for (int i=0; i < grid.hauteur; i++) {
			for (int j=0; j < grid.largeur; j++) {
				if (grid.grille[i][j] != null) {
					renderer.setColor(grid.grille[i][j].getCouleur());
					renderer.circle(grid.grille[i][j].animX, grid.grille[i][j].animY, CIRCLE_RAD);
				}
			}
		}
	}

	private void drawText() {
		for (int i=0; i < grid.hauteur; i++) {
			for (int j=0; j < grid.largeur; j++) {
				if (grid.grille[i][j] != null) {
					game.font.getData().setScale(.7f);
					game.font.draw(game.batch, grid.grille[i][j].getC() + ";" + grid.grille[i][j].getL()  , grid.grille[i][j].animX - CIRCLE_RAD/2, grid.grille[i][j].animY );
				}
			}
		}
	}

	private void moveCandies() {
		Iterator<Bonbon> itr = movingCandies.iterator();
		while (itr.hasNext()) {
			Bonbon current = (Bonbon) itr.next();
			if (hasArrived(current)) {
				endAnimation(current);
				itr.remove();
			} else {
				moveCandy(current);
			}
		}
	}

	private boolean hasArrived(Bonbon bb) {
		double dist = Math.sqrt(Math.pow(bb.getC() - bb.animX,2) + Math.pow(bb.getL() - bb.animY,2));
		if (dist < 1) return true;
		return false;
	}

	private void moveCandy(Bonbon bb) {


		
		bb.animX += Gdx.graphics.getDeltaTime() * SPEED * (bb.getC() - bb.animX);
		bb.animY += Gdx.graphics.getDeltaTime() * SPEED * (bb.getL() - bb.animY);
		/*
		double normX = 1 - Math.abs(bb.animX-bb.getC())/70;
		double normY = 1 - Math.abs(bb.animY-bb.getC())/70;
		bb.animX +=  bb.getC() * c3 * normX * normX * normX - c1 * normX * normX;
		bb.animY +=  bb.getL() * c3 * normY * normY * normY - c1 * normY * normY;
		*/
	}

	private void endAnimation(Bonbon bb) {
		bb.animX = bb.getC();
		bb.animY = bb.getL();
	}

	private void startBegginingAnimation() {
		for (int i=0; i < grid.hauteur; i++) {
			for (int j=0; j < grid.largeur; j++) {
				grid.grille[i][j].animY = 0;
				movingCandies.add(grid.grille[i][j]);
			}
		}
	}
/*
	// lance l'animation de la destruction d'un bonbon (il faut l'utliser avant l'appel a la fonction detruire)
	private void destroyAndPlayExplosion(Bonbon b1) {
		new Explosion(game, b1.getC()-90, b1.getL()-90, CIRCLE_RAD*6);
		grid.destroyCandyInGrid(b1);
	}
*/
	private void runMoveAnimationOnCol(int i) {
		for (int k=0 ; k<grid.hauteur; k++) {
			if (grid.grille[k][i] != null) {
				movingCandies.add(grid.grille[k][i]);
			}
		}
	}

	private void descendreCol(int i) {
		grid.remplirecol(i);
		runMoveAnimationOnCol(i);
	}

	

	//lance l'animation de la descente des bonbons + le remplissage des cases vides
	//private void moveAndFillAnimation(){

	//}


    @Override
    public void resize(int width, int height) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void pause() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void resume() {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void hide() {
        // TODO Auto-generated method stub
        
    }

    
    
    @Override
    public void show() {
        // TODO Auto-generated method stub
        
    }

	public void destroyNeighbors(Bonbon b1, ArrayList<Integer> ls) {
		int i = grid.transformerEnCoordonneeGrille(b1.getL());
		int j = grid.transformerEnCoordonneeGrille(b1.getC());
		int g = ls.get(0); 
		int d = ls.get(1); 
		int h = ls.get(2);
		int b = ls.get(3);
		if (g+d > 1) {
			for ( int k = j-g; k < j+d+1; k++) {
				new Explosion(game, (k+1)*grid.SPACING-90, (i+1)*grid.SPACING-90, CIRCLE_RAD*6);
			}
		}
		if (h+b > 1) {
			for (int k = i-b; k<i+h+1; k++) {	
				new Explosion(game, (j+1)*grid.SPACING-90, (k+1)*grid.SPACING-90, CIRCLE_RAD*6);
			}
		}
		
	} 


}
