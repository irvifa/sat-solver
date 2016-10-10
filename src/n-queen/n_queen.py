import sys
import os

class NQueen(object):
	def __init__(self, sizeOfBoard, cnfFileName, outputFileName):
		self.sizeOfBoard = sizeOfBoard
		self.cnfFileName = cnfFileName
		self.outputFileName = outputFileName
	
	"""
	rule generator
	"""
	def gen_rules(self):
		cnfFile= open(self.cnfFileName,"w")
		
		for i in range(0,self.sizeOfBoard):
		 tmp=''
		 for j in range(1,self.sizeOfBoard+1):
		  number=str(j+self.sizeOfBoard*i)
		  tmp=tmp+number+" "
		 tmp=tmp+" 0\n"
		 cnfFile.write(tmp)

		#constraints on rows
		for i in range(0,self.sizeOfBoard):
		 for j in range(1,self.sizeOfBoard):
		   number=j+self.sizeOfBoard*i
		   for l in range(1,self.sizeOfBoard-j+1):
			tmp="-"+str(number)+" -"+str(number+l)+" 0\n"
			cnfFile.write(tmp)
		
		#constraints on columns
		for j in range(1,self.sizeOfBoard+1):
		 for i in range(0,self.sizeOfBoard):
		   number=j+self.sizeOfBoard*i;
		   for l in range(1,self.sizeOfBoard-i):
			tmp="-"+str(number)+" -"+str(number+self.sizeOfBoard*l)+" 0\n"
			cnfFile.write(tmp)

		#constraints on NW->SE diagonal
		# part 1, upper bound triangle
		for i in range(0,self.sizeOfBoard-1):
		 for j in range(i,self.sizeOfBoard-1):
		  number=j+1+self.sizeOfBoard*i
		  for l in range(1,self.sizeOfBoard-j):
			tmp="-"+str(number)+" -"+str(number+l*(self.sizeOfBoard+1))+" 0\n"
			cnfFile.write(tmp)

		# part 2, lower bound triangle
		for i in range(0,self.sizeOfBoard-1):
		 for j in range(0,i):
		  number=j+1+self.sizeOfBoard*i
		  for l in range(1,self.sizeOfBoard-i):
		   tmp="-"+str(number)+" -"+str(number+l*(self.sizeOfBoard+1))+" 0\n"
		   cnfFile.write(tmp)

		#constraints on NE->SW diagonal
		# part 1, upper bound triangle
		for i in range(0,self.sizeOfBoard):
		 for j in range(0,self.sizeOfBoard-i):
		  number=j+1+self.sizeOfBoard*i
		  for l in range(1,j+1):
		   tmp="-"+str(number)+" -"+str(number+l*(self.sizeOfBoard-1))+" 0\n"
		   cnfFile.write(tmp);

		# part 2, lower bound triangle
		for i in range(0,self.sizeOfBoard):
		 for j in range(self.sizeOfBoard-i,self.sizeOfBoard):
		  number=j+1+self.sizeOfBoard*i;
		  if (number != self.sizeOfBoard*self.sizeOfBoard ):
		   for l in range(1,self.sizeOfBoard-i):
			tmp="-"+str(number)+" -"+str(number+l*(self.sizeOfBoard-1))+" 0\n"
			cnfFile.write(tmp)
			
		cnfFile.close();
	
	def solve(self):
		outputFile=open(self.outputFileName,"w");
		exe="./../minisat/core/minisat "+self.cnfFileName+" "+self.outputFileName
		os.system(exe);
	
	def convert_to_board(self):
		ansfilename="ans"
		ans=open(ansfilename,"w");
		chan = open(self.outputFileName).read().split()
		chan.pop(0)
		for i in range(0,self.sizeOfBoard):
		 row = ""
		 for j in range(0,self.sizeOfBoard):
		  row+= chan[i*self.sizeOfBoard+j] + " "
		 row+="\n"
		 ans.write(row)

		ans.close(); 
		 
def main():
	n=int(sys.argv[1]);

	if (len(sys.argv)<2):
	 print "Usage: python queen.py <sizeOfBoard>  "
	 print "Or, just: python queen.py "
	 exit();

	cnfFileName="rules.cnf"
	outputFileName="result"
	
	nq = NQueen(n,cnfFileName,outputFileName)
	
	nq.gen_rules()
	nq.solve()
	nq.convert_to_board()

if __name__=="__main__":
	main()

