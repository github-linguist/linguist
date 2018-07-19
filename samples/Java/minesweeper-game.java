This is a version of minesweeper with a gui. The code might not be optimal, but at least its not hard to understand.

//--------------------------------- START of Main.java ---------------------------------

/*
 * Main.java
 *
 * Created by Potrik
 * Last modified: 07.22.13
 */

public class Main
{
	public static void main (String[] args)
	{
		int x = 10;		//Width of the board
		int y = 10;		//Height of the board
		int d = 25;		//The difficulty of the game, the percentage of mines in the board. The number of mines per board is random, but this number is the probability that a cell will become
					//a mine.
		
		new Minesweeper(x, y, d);
	}
}

//--------------------------------- END of Main.java ---------------------------------

//--------------------------------- START of Cell.java ---------------------------------

/*
 * Cell.java
 *
 * Created by Potrik
 * Last modified: 07.22.13
 */

public class Cell
{
	private boolean isMine, isFlagged, isObscured;
	private int number;
	
	public Cell()
	{
		isMine = false;
		isFlagged = false;
		isObscured = true;
		number = 0;
	}
	
	public void setMine()
	{
		isMine = true;
	}
	
	public void flag()
	{
		isFlagged = true;
	}
	
	public void unflag()
	{
		isFlagged = false;
	}
	
	public void reveal()
	{
		isObscured = false;
	}
	
	public void setNumber(int i)
	{
		number = i;
	}
	
	public boolean isMine()
	{
		return isMine;
	}
	
	public boolean isFlagged()
	{
		return isFlagged;
	}
	
	public boolean isObscured()
	{
		return isObscured;
	}
	
	public int getNumber()
	{
		return number;
	}
}

//--------------------------------- END of Cell.java ---------------------------------

//--------------------------------- START of Board.java ---------------------------------

/*
 * Board.java
 *
 * Created by Potrik
 * Last modified: 07.22.13
 */

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import javax.swing.JPanel;

public class Board extends JPanel
{
	private static final long serialVersionUID = 1L;
	private Minesweeper mine;
	private Cell[][] cells;
	
	public Board(Minesweeper m)
	{
		mine = m;
		cells = mine.getCells();
		
		addMouseListener(new Actions(mine));
		
		setPreferredSize(new Dimension(mine.getx() * 20, mine.gety() * 20));
	}
	
	public void paintComponent(Graphics g)
	{
		cells = mine.getCells();
		
		for (int i = 0; i < mine.getx(); i++)
		{
			for (int j = 0; j < mine.gety(); j++)
			{
				Cell current = cells[i][j];

				if (current.isFlagged())
				{
					if (current.isMine() && mine.isFinished())
					{
						g.setColor(Color.ORANGE);
						g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
						g.setColor(Color.BLACK);
						
						g.drawLine(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
						g.drawLine(i * 20, j * 20 + 20, i * 20 + 20, j * 20);
					}
					else if (mine.isFinished())
					{
						g.setColor(Color.GREEN);
						g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
						g.setColor(Color.BLACK);
					}
					else
					{
						g.setColor(Color.YELLOW);
						g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
						g.setColor(Color.BLACK);
					}
				}
				else if (current.isObscured())
				{
					g.setColor(Color.GRAY);
					g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
					g.setColor(Color.BLACK);
				}
				else if (current.isMine())
				{
					g.setColor(Color.RED);
					g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
					g.setColor(Color.BLACK);
					g.drawLine(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
					g.drawLine(i * 20, j * 20 + 20, i * 20 + 20, j * 20);
				}
				else
				{
					g.setColor(Color.LIGHT_GRAY);
					g.fillRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
					g.setColor(Color.BLACK);
				}
				if (!current.isObscured())
				{
					if (current.getNumber() == 1)
					{
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
					}
					else if (current.getNumber() == 2)
					{
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 7, j * 20 + 11, i * 20 + 7, j * 20 + 15);	//5
						g.drawLine(i * 20 + 8, j * 20 + 16, i * 20 + 12, j * 20 + 16);	//7
					}
					else if (current.getNumber() == 3)
					{
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
						g.drawLine(i * 20 + 8, j * 20 + 16, i * 20 + 12, j * 20 + 16);	//7
					}
					else if (current.getNumber() == 4)
					{
						g.drawLine(i * 20 + 7, j * 20 + 5, i * 20 + 7, j * 20 + 9);		//1
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
					}
					else if (current.getNumber() == 5)
					{
						g.drawLine(i * 20 + 7, j * 20 + 5, i * 20 + 7, j * 20 + 9);		//1
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
						g.drawLine(i * 20 + 8, j * 20 + 16, i * 20 + 12, j * 20 + 16);	//7
					}
					else if (current.getNumber() == 6)
					{
						g.drawLine(i * 20 + 7, j * 20 + 5, i * 20 + 7, j * 20 + 9);		//1
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 7, j * 20 + 11, i * 20 + 7, j * 20 + 15);	//5
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
						g.drawLine(i * 20 + 8, j * 20 + 16, i * 20 + 12, j * 20 + 16);	//7
					}
					else if (current.getNumber() == 7)
					{
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
					}
					else if (current.getNumber() == 8)
					{
						g.drawLine(i * 20 + 7, j * 20 + 5, i * 20 + 7, j * 20 + 9);		//1
						g.drawLine(i * 20 + 8, j * 20 + 4, i * 20 + 12, j * 20 + 4);	//2
						g.drawLine(i * 20 + 13, j * 20 + 5, i * 20 + 13, j * 20 + 9);	//3
						g.drawLine(i * 20 + 8, j * 20 + 10, i * 20 + 12, j * 20 + 10);	//4
						g.drawLine(i * 20 + 7, j * 20 + 11, i * 20 + 7, j * 20 + 15);	//5
						g.drawLine(i * 20 + 13, j * 20 + 11, i * 20 + 13, j * 20 + 15);	//6
						g.drawLine(i * 20 + 8, j * 20 + 16, i * 20 + 12, j * 20 + 16);	//7
					}
				}
				g.setColor(Color.BLACK);
				g.drawRect(i * 20, j * 20, i * 20 + 20, j * 20 + 20);
			}
		}
	}
}

//--------------------------------- END of Board.java ---------------------------------

//--------------------------------- START of Actions.java ---------------------------------

/*
 * Board.java
 *
 * Created by Potrik
 * Last modified: 07.22.13
 */

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

public class Actions implements ActionListener, MouseListener
{
	private Minesweeper mine;
	
	public Actions(Minesweeper m)
	{
		mine = m;
	}
	
	public void actionPerformed(ActionEvent e)
	{	
		mine.reset();
		
		mine.refresh();
	}
	
	public void mouseClicked(MouseEvent e)
	{
		if (e.getButton() == 1)
		{
			int x = e.getX() / 20;
			int y = e.getY() / 20;
			
			mine.select(x, y);
		}
		
		if (e.getButton() == 3)
		{
			int x = e.getX() / 20;
			int y = e.getY() / 20;
			
			mine.mark(x, y);
		}
		
		mine.refresh();
	}

	public void mouseEntered(MouseEvent e)
	{

	}

	public void mouseExited(MouseEvent e)
	{

	}

	public void mousePressed(MouseEvent e)
	{

	}

	public void mouseReleased(MouseEvent e)
	{

	}

}

//--------------------------------- END of Actions.java ---------------------------------

//--------------------------------- START of Minesweeper.java ---------------------------------

/*
 * Minesweeper.java
 *
 * Created by Potrik
 * Last modified: 07.22.13
 */

import java.awt.BorderLayout;
import java.util.Random;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

public class Minesweeper extends JFrame
{
	private static final long serialVersionUID = 1L;
	private int width, height;
	private Cell[][] cells;
	private int difficulty;
	private Board board;
	private JButton reset;
	private boolean finished;
	
	public Minesweeper(int x, int y, int d)
	{
		width = x;
		height = y;
		difficulty = d;
		cells = new Cell[width][height];
		
		reset();
		
		board = new Board(this);
		reset = new JButton("Reset");
		
		add(board, BorderLayout.CENTER);
		add(reset, BorderLayout.SOUTH);
		
		reset.addActionListener(new Actions(this));
		
		setTitle("Minesweeper");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setResizable(false);
		pack();
		setVisible(true);
	}
	
	public int getx()
	{
		return width;
	}
	
	public int gety()
	{
		return height;
	}
	
	public Cell[][] getCells()
	{
		return cells;
	}
	
	public void reset()
	{
		Random random = new Random();
		finished = false;
		
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				Cell c = new Cell();
				cells[i][j] = c;
				int r = random.nextInt(100);
				
				if (r < difficulty)
				{
					cells[i][j].setMine();
				}
			}
		}
		setNumbers();
	}
	
	private void setNumbers()
	{
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				int count = 0;
				
				if (i > 0 &&	j > 0 && cells[i - 1]	[j - 1]	.isMine()) count++;
				if (j > 0 && cells[i][j - 1].isMine()) count++;
				if (i < width - 1 && j > 0 && cells[i + 1][j - 1].isMine()) count++;
				
				if (i > 0 && cells[i - 1][j].isMine()) count++;
				if (i < width - 1 && cells[i + 1][j].isMine()) count++;
				
				if (i > 0 && j < height - 1 && cells[i - 1][j + 1].isMine()) count++;
				if (j < height - 1	&& cells[i] [j + 1].isMine()) count++;
				if (i < width - 1 && j < height - 1 && cells[i + 1][j + 1].isMine()) count++;
				
				cells[i][j].setNumber(count);
				
				if (cells[i][j].isMine())
				{
					cells[i][j].setNumber(-1);
				}
				
				if (cells[i][j].getNumber() == 0)
				{
					cells[i][j].reveal();
				}
			}
		}
		
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				if (i > 0 &&	j > 0 && cells[i - 1][j - 1].getNumber() == 0) cells[i][j].reveal();
				if (j > 0 && cells[i][j - 1].getNumber() == 0) cells[i][j].reveal();
				if (i < width - 1 && j > 0 && cells[i + 1][j - 1].getNumber() == 0) cells[i][j].reveal();
				
				if (i > 0 && cells[i - 1][j].getNumber() == 0) cells[i][j].reveal();
				if (i < width - 1 && cells[i + 1]	[j]		.getNumber() == 0) cells[i][j].reveal();
				
				if (i > 0 && j < height - 1 && cells[i - 1][j + 1].getNumber() == 0) cells[i][j].reveal();
				if (j < height - 1 && cells[i][j + 1].getNumber() == 0) cells[i][j].reveal();
				if (i < width - 1 && j < height - 1 && cells[i + 1][j + 1]	.getNumber() == 0) cells[i][j].reveal();
			}
		}
	}
	
	public void refresh()
	{
		board.repaint();
	}
	
	public void select(int x, int y)
	{
		if (cells[x][y].isFlagged()) return;
		cells[x][y].reveal();
		resetMarks();
		refresh();
		
		if (cells[x][y].isMine())
		{
			loose();
		}
		else if (won())
		{
			win();
		}
	}
	
	private void loose()
	{
		finished = true;
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				if (!cells[i][j].isObscured()) cells[i][j].unflag();
				cells[i][j].reveal();
			}
		}
		refresh();
		JOptionPane.showMessageDialog(null, "BOOOOM!");
		reset();
	}
	
	private void win()
	{
		finished = true;
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				cells[i][j].reveal();
				if (!cells[i][j].isMine()) cells[i][j].unflag();
			}
		}
		
		refresh();
		JOptionPane.showMessageDialog(null, "Congratulations! You won!");
		reset();
	}
	
	private boolean won()
	{
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				if (cells[i][j].isObscured() && !cells[i][j].isMine())
				{
					return false;
				}
			}
		}
		
		return true;
	}
	
	public void mark(int x, int y)
	{
		if (cells[x][y].isFlagged()) cells[x][y].unflag();
		else if (cells[x][y].isObscured()) cells[x][y].flag();
		
		resetMarks();
	}
	
	private void resetMarks()
	{
		for (int i = 0; i < width; i++)
		{
			for (int j = 0; j < height; j++)
			{
				if (!cells[i][j].isObscured()) cells[i][j].unflag();
			}
		}
	}
	
	public boolean isFinished()
	{
		return finished;
	}
}

//--------------------------------- END of Minesweeper.java ---------------------------------
